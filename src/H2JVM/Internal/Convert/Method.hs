{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE RecordWildCards #-}

-- | Convert a high level method to its low level format.
module H2JVM.Internal.Convert.Method (convertMethod, convertMethodDescriptor) where

import Control.Monad (foldM)
import Data.List (nubBy, sortOn)
import Data.Word (Word16)
import Effectful
import GHC.Stack (HasCallStack)
import Witch

import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as V

import H2JVM.ClassFile.Method
import H2JVM.ConstantPool (ConstantPoolEntry (..))
import H2JVM.Internal.Convert.AccessFlag (accessFlagsToWord16)
import H2JVM.Internal.Convert.ConstantPool
import H2JVM.Internal.Convert.Instruction (CodeConverterEff, convertInstructions, fullyResolveAbs, fullyRunCodeConverter)
import H2JVM.Internal.Convert.Monad
import H2JVM.Internal.Convert.Type
import H2JVM.Internal.Raw.Types

import H2JVM.ClassFile.Method qualified as Abs
import H2JVM.Data.TypeMergingList qualified as TML
import H2JVM.Internal.Raw.ClassFile qualified as Raw

-- | Convert a 'Abs.MethodAttribute' to the raw format by upserting its components into the constant pool.
convertMethodAttribute :: ConvertEff r => HasCallStack => Abs.MethodAttribute -> Eff r Raw.AttributeInfo
convertMethodAttribute (Abs.Code (Abs.CodeAttributeData{..})) = do
    (code', attributes') <- fullyRunCodeConverter $ do
        liftA2 (,) (convertInstructions code) (convertCodeAttributes codeAttributes)
    exceptionTable' <- convertExceptionTable exceptionTable

    nameIndex <- findIndexOf (CPUTF8Entry "Code")

    pure $
        Raw.AttributeInfo
            (into nameIndex)
            ( Raw.CodeAttribute
                maxStack
                maxLocals
                (V.fromList $ NE.toList code')
                exceptionTable'
                attributes'
            )
  where
    convertExceptionTable :: ConvertEff r => [Abs.ExceptionTableEntry] -> Eff r (V.Vector Raw.ExceptionTableEntry)
    convertExceptionTable = fmap V.fromList . traverse convertExceptionTableEntry

    convertExceptionTableEntry :: Abs.ExceptionTableEntry -> Eff r Raw.ExceptionTableEntry
    convertExceptionTableEntry = undefined

    convertCodeAttributes :: CodeConverterEff r => [Abs.CodeAttribute] -> Eff r (V.Vector Raw.AttributeInfo)
    convertCodeAttributes = fmap V.fromList . traverse convertCodeAttribute'

    convertCodeAttribute' :: CodeConverterEff r => Abs.CodeAttribute -> Eff r Raw.AttributeInfo
    convertCodeAttribute' (LineNumberTable lns) = do
        lns' <- convertLineNumberTable lns
        nameIndex <- findIndexOf (CPUTF8Entry "LineNumberTable")
        pure $ Raw.AttributeInfo (into nameIndex) (Raw.LineNumberTableAttribute lns')
      where
        convertLineNumberTable :: [Abs.LineNumberTableEntry] -> Eff r (V.Vector Raw.LineNumberTableEntry)
        convertLineNumberTable = fmap V.fromList . traverse convertLineNumberTableEntry

        convertLineNumberTableEntry :: Abs.LineNumberTableEntry -> Eff r Raw.LineNumberTableEntry
        convertLineNumberTableEntry (Abs.LineNumberTableEntry a b) = pure $ Raw.LineNumberTableEntry a b
    convertCodeAttribute' (StackMapTable frames) = do
        frames' <- convertStackMapTable frames
        nameIndex <- findIndexOf (CPUTF8Entry "StackMapTable")
        pure $ Raw.AttributeInfo (into nameIndex) (Raw.StackMapTableAttribute frames')
      where
        convertStackMapTable :: CodeConverterEff r => [Abs.StackMapFrame] -> Eff r (V.Vector Raw.StackMapFrame)
        convertStackMapTable fs = do
            resolvedFrames <- traverse resolveFrame fs
            let uniqueFrames =
                    nubBy (\(o1, _) (o2, _) -> o1 == o2) $
                        sortOn fst resolvedFrames
            V.fromList . reverse . snd <$> foldM convertAndAccumulate (-1, []) uniqueFrames

        resolveFrame :: CodeConverterEff r => Abs.StackMapFrame -> Eff r (Word16, Abs.StackMapFrame)
        resolveFrame f = do
            off <- fullyResolveAbs (getFrameLabel f)
            pure (off, f)

        getFrameLabel (Abs.SameFrame l) = l
        getFrameLabel (Abs.ChopFrame _ l) = l
        getFrameLabel (Abs.SameLocals1StackItemFrame _ l) = l
        getFrameLabel (Abs.AppendFrame _ l) = l
        getFrameLabel (Abs.FullFrame _ _ l) = l

        convertAndAccumulate :: CodeConverterEff r => (Int, [Raw.StackMapFrame]) -> (Word16, Abs.StackMapFrame) -> Eff r (Int, [Raw.StackMapFrame])
        convertAndAccumulate (prev, acc) (absLabel16, frame) = do
            let current = into absLabel16
            let delta = unsafeInto @U1 (current - prev - 1)

            rawFrame <- case frame of
                Abs.SameFrame _ ->
                    pure $
                        if delta <= 63
                            then Raw.SameFrame delta
                            else Raw.SameFrameExtended (into delta)
                Abs.ChopFrame x _ ->
                    pure $ Raw.ChopFrame x (into delta)
                Abs.SameLocals1StackItemFrame x _ -> do
                    x' <- convertVerificationTypeInfo x
                    pure $
                        if delta <= 63
                            then Raw.SameLocals1StackItemFrame x' delta
                            else Raw.SameLocals1StackItemFrameExtended x' (into delta)
                Abs.AppendFrame x _ -> do
                    x' <- traverse convertVerificationTypeInfo x
                    pure $ Raw.AppendFrame (V.fromList x') (into delta)
                Abs.FullFrame x y _ -> do
                    x' <- traverse convertVerificationTypeInfo x
                    y' <- traverse convertVerificationTypeInfo y
                    pure $ Raw.FullFrame (V.fromList x') (V.fromList y') (into delta)

            pure (current, rawFrame : acc)

convertVerificationTypeInfo :: CodeConverterEff r => Abs.VerificationTypeInfo -> Eff r Raw.VerificationTypeInfo
convertVerificationTypeInfo Abs.TopVariableInfo = pure Raw.TopVariableInfo
convertVerificationTypeInfo Abs.IntegerVariableInfo = pure Raw.IntegerVariableInfo
convertVerificationTypeInfo Abs.FloatVariableInfo = pure Raw.FloatVariableInfo
convertVerificationTypeInfo Abs.LongVariableInfo = pure Raw.LongVariableInfo
convertVerificationTypeInfo Abs.DoubleVariableInfo = pure Raw.DoubleVariableInfo
convertVerificationTypeInfo Abs.NullVariableInfo = pure Raw.NullVariableInfo
convertVerificationTypeInfo Abs.UninitializedThisVariableInfo = pure Raw.UninitializedThisVariableInfo
convertVerificationTypeInfo (Abs.ObjectVariableInfo x) = do
    cpIndex <- findIndexOf (CPClassEntry x)
    pure $ Raw.ObjectVariableInfo (into cpIndex)
convertVerificationTypeInfo (Abs.UninitializedVariableInfo x) = do
    label <- fullyResolveAbs x
    pure $ Raw.UninitializedVariableInfo (into label)

-- | Convert a 'Abs.ClassFileMethod' to a 'Raw.MethodInfo' by upserting its attributes into the constant pool.
convertMethod :: ConvertEff r => Abs.ClassFileMethod -> Eff r Raw.MethodInfo
convertMethod Abs.ClassFileMethod{..} = do
    let flags = accessFlagsToWord16 methodAccessFlags
    nameIndex <- findIndexOf (CPUTF8Entry methodName)
    descriptorIndex <- findIndexOf (CPUTF8Entry (convertMethodDescriptor methodDescriptor))
    attributes <- traverse convertMethodAttribute (TML.toVector methodAttributes)
    pure $ Raw.MethodInfo flags (into nameIndex) (into descriptorIndex) attributes
