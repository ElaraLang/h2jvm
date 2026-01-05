{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE RecordWildCards #-}

module JVM.Data.Convert.Method where

import Control.Applicative (liftA2)
import Control.Monad (foldM)
import Data.List (mapAccumL, nubBy, sortOn)
import Data.TypeMergingList qualified as TML
import Data.Vector qualified as V
import Data.Word (Word16)
import Effectful
import GHC.Stack (HasCallStack)
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.ClassFile.Method qualified as Abs
import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (..))
import JVM.Data.Convert.AccessFlag (accessFlagsToWord16)
import JVM.Data.Convert.ConstantPool
import JVM.Data.Convert.Descriptor (convertMethodDescriptor)
import JVM.Data.Convert.Instruction (CodeConverterEff, convertInstructions, fullyResolveAbs, fullyRunCodeConverter)
import JVM.Data.Convert.Monad
import JVM.Data.Raw.ClassFile qualified as Raw
import JVM.Data.Raw.Types

-- >>> foldMWith (\a b -> pure (a + b, a + b)) 0 [1, 2, 3]
-- (6,[1,3,6])
foldMWith :: (Monad m) => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
foldMWith _ a [] = pure (a, [])
foldMWith f a (x : xs) = do
    (a', x') <- f a x
    (a'', xs') <- foldMWith f a' xs
    pure (a'', x' : xs')

convertMethodAttribute :: (ConvertEff r) => (HasCallStack) => Abs.MethodAttribute -> Eff r Raw.AttributeInfo
convertMethodAttribute (Abs.Code (Abs.CodeAttributeData{..})) = do
    (code', attributes') <- fullyRunCodeConverter $ do
        liftA2 (,) (convertInstructions code) (convertCodeAttributes codeAttributes)
    exceptionTable' <- convertExceptionTable exceptionTable

    nameIndex <- findIndexOf (CPUTF8Entry "Code")

    pure $ Raw.AttributeInfo (fromIntegral nameIndex) (Raw.CodeAttribute maxStack maxLocals (V.fromList code') exceptionTable' attributes')
  where
    convertExceptionTable :: (ConvertEff r) => [Abs.ExceptionTableEntry] -> Eff r (V.Vector Raw.ExceptionTableEntry)
    convertExceptionTable = fmap V.fromList . traverse convertExceptionTableEntry

    convertExceptionTableEntry :: Abs.ExceptionTableEntry -> Eff r Raw.ExceptionTableEntry
    convertExceptionTableEntry = undefined

    convertCodeAttributes :: (CodeConverterEff r) => [Abs.CodeAttribute] -> Eff r (V.Vector Raw.AttributeInfo)
    convertCodeAttributes = fmap V.fromList . traverse convertCodeAttribute'

    convertCodeAttribute' :: (CodeConverterEff r) => Abs.CodeAttribute -> Eff r Raw.AttributeInfo
    convertCodeAttribute' (LineNumberTable lns) = do
        lns' <- convertLineNumberTable lns
        nameIndex <- findIndexOf (CPUTF8Entry "LineNumberTable")
        pure $ Raw.AttributeInfo (fromIntegral nameIndex) (Raw.LineNumberTableAttribute lns')
      where
        convertLineNumberTable :: [Abs.LineNumberTableEntry] -> Eff r (V.Vector Raw.LineNumberTableEntry)
        convertLineNumberTable = fmap V.fromList . traverse convertLineNumberTableEntry

        convertLineNumberTableEntry :: Abs.LineNumberTableEntry -> Eff r Raw.LineNumberTableEntry
        convertLineNumberTableEntry (Abs.LineNumberTableEntry a b) = pure $ Raw.LineNumberTableEntry a b
    convertCodeAttribute' (StackMapTable frames) = do
        frames' <- convertStackMapTable frames
        nameIndex <- findIndexOf (CPUTF8Entry "StackMapTable")
        pure $ Raw.AttributeInfo (fromIntegral nameIndex) (Raw.StackMapTableAttribute frames')
      where
        convertStackMapTable :: (CodeConverterEff r) => [Abs.StackMapFrame] -> Eff r (V.Vector Raw.StackMapFrame)
        convertStackMapTable fs = do
            resolvedFrames <- traverse resolveFrame fs
            let uniqueFrames =
                    nubBy (\(o1, _) (o2, _) -> o1 == o2) $
                        sortOn fst resolvedFrames
            V.fromList . reverse . snd <$> foldM convertAndAccumulate (-1, []) uniqueFrames

        resolveFrame :: (CodeConverterEff r) => Abs.StackMapFrame -> Eff r (Word16, Abs.StackMapFrame)
        resolveFrame f = do
            off <- fullyResolveAbs (getFrameLabel f)
            pure (off, f)

        getFrameLabel (Abs.SameFrame l) = l
        getFrameLabel (Abs.ChopFrame _ l) = l
        getFrameLabel (Abs.SameLocals1StackItemFrame _ l) = l
        getFrameLabel (Abs.AppendFrame _ l) = l
        getFrameLabel (Abs.FullFrame _ _ l) = l

        convertAndAccumulate :: (CodeConverterEff r) => (Int, [Raw.StackMapFrame]) -> (Word16, Abs.StackMapFrame) -> Eff r (Int, [Raw.StackMapFrame])
        convertAndAccumulate (prev, acc) (absLabel16, frame) = do
            let current = fromIntegral absLabel16
            let delta = current - prev - 1

            rawFrame <- case frame of
                Abs.SameFrame _ ->
                    pure $
                        if delta <= 63
                            then Raw.SameFrame (fromIntegral delta)
                            else Raw.SameFrameExtended (fromIntegral delta)
                Abs.ChopFrame x _ ->
                    pure $ Raw.ChopFrame x (fromIntegral delta)
                Abs.SameLocals1StackItemFrame x _ -> do
                    x' <- convertVerificationTypeInfo x
                    pure $
                        if delta <= 63
                            then Raw.SameLocals1StackItemFrame x' (fromIntegral delta)
                            else Raw.SameLocals1StackItemFrameExtended x' (fromIntegral delta)
                Abs.AppendFrame x _ -> do
                    x' <- traverse convertVerificationTypeInfo x
                    pure $ Raw.AppendFrame (V.fromList x') (fromIntegral delta)
                Abs.FullFrame x y _ -> do
                    x' <- traverse convertVerificationTypeInfo x
                    y' <- traverse convertVerificationTypeInfo y
                    pure $ Raw.FullFrame (V.fromList x') (V.fromList y') (fromIntegral delta)

            -- Check for overflow explicitly if you want, otherwise fromIntegral handles it
            pure (current, rawFrame : acc)

convertVerificationTypeInfo :: (CodeConverterEff r) => Abs.VerificationTypeInfo -> Eff r Raw.VerificationTypeInfo
convertVerificationTypeInfo Abs.TopVariableInfo = pure Raw.TopVariableInfo
convertVerificationTypeInfo Abs.IntegerVariableInfo = pure Raw.IntegerVariableInfo
convertVerificationTypeInfo Abs.FloatVariableInfo = pure Raw.FloatVariableInfo
convertVerificationTypeInfo Abs.LongVariableInfo = pure Raw.LongVariableInfo
convertVerificationTypeInfo Abs.DoubleVariableInfo = pure Raw.DoubleVariableInfo
convertVerificationTypeInfo Abs.NullVariableInfo = pure Raw.NullVariableInfo
convertVerificationTypeInfo Abs.UninitializedThisVariableInfo = pure Raw.UninitializedThisVariableInfo
convertVerificationTypeInfo (Abs.ObjectVariableInfo x) = do
    cpIndex <- findIndexOf (CPClassEntry x)
    pure $ Raw.ObjectVariableInfo (fromIntegral cpIndex)
convertVerificationTypeInfo (Abs.UninitializedVariableInfo x) = do
    label <- fullyResolveAbs x
    pure $ Raw.UninitializedVariableInfo (fromIntegral label)

convertMethod :: (ConvertEff r) => Abs.ClassFileMethod -> Eff r Raw.MethodInfo
convertMethod Abs.ClassFileMethod{..} = do
    let flags = accessFlagsToWord16 methodAccessFlags
    nameIndex <- findIndexOf (CPUTF8Entry methodName)
    descriptorIndex <- findIndexOf (CPUTF8Entry (convertMethodDescriptor methodDescriptor))
    attributes <- traverse convertMethodAttribute (TML.toVector methodAttributes)
    pure $ Raw.MethodInfo flags (fromIntegral nameIndex) (fromIntegral descriptorIndex) attributes
