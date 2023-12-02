{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE RecordWildCards #-}

module JVM.Data.Convert.Method where

import Control.Applicative (liftA2)
import Data.TypeMergingList qualified as TML
import Data.Vector qualified as V
import GHC.Stack (HasCallStack)
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.ClassFile.Method qualified as Abs
import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (..))
import JVM.Data.Convert.AccessFlag (accessFlagsToWord16)
import JVM.Data.Convert.ConstantPool
import JVM.Data.Convert.Descriptor (convertMethodDescriptor)
import JVM.Data.Convert.Instruction (CodeConverter, convertInstructions, fullyResolveAbs, fullyRunCodeConverter)
import JVM.Data.Convert.Monad (ConvertM)
import JVM.Data.Raw.ClassFile qualified as Raw
import JVM.Data.Raw.Types

-- >>> foldMWith (\a b -> pure (a + b, a + b)) 0 [1, 2, 3]
-- (6,[1,3,6])
foldMWith :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
foldMWith _ a [] = pure (a, [])
foldMWith f a (x : xs) = do
    (a', x') <- f a x
    (a'', xs') <- foldMWith f a' xs
    pure (a'', x' : xs')

convertMethodAttribute :: HasCallStack => Abs.MethodAttribute -> ConvertM Raw.AttributeInfo
convertMethodAttribute (Abs.Code (Abs.CodeAttributeData{..})) = do
    (code', attributes') <- fullyRunCodeConverter $ do
        liftA2 (,) (convertInstructions code) (convertCodeAttributes codeAttributes)
    exceptionTable' <- convertExceptionTable exceptionTable

    nameIndex <- findIndexOf (CPUTF8Entry "Code")

    pure $ Raw.AttributeInfo (fromIntegral nameIndex) (Raw.CodeAttribute maxStack maxLocals (V.fromList code') exceptionTable' attributes')
  where
    convertExceptionTable :: [Abs.ExceptionTableEntry] -> ConvertM (V.Vector Raw.ExceptionTableEntry)
    convertExceptionTable = fmap V.fromList . traverse convertExceptionTableEntry

    convertExceptionTableEntry :: Abs.ExceptionTableEntry -> ConvertM Raw.ExceptionTableEntry
    convertExceptionTableEntry = undefined

    convertCodeAttributes :: [Abs.CodeAttribute] -> CodeConverter (V.Vector Raw.AttributeInfo)
    convertCodeAttributes = fmap V.fromList . traverse convertCodeAttribute'

    convertCodeAttribute' :: Abs.CodeAttribute -> CodeConverter Raw.AttributeInfo
    convertCodeAttribute' (LineNumberTable lns) = do
        lns' <- convertLineNumberTable lns
        nameIndex <- findIndexOf (CPUTF8Entry "LineNumberTable")
        pure $ Raw.AttributeInfo (fromIntegral nameIndex) (Raw.LineNumberTableAttribute lns')
      where
        convertLineNumberTable :: [Abs.LineNumberTableEntry] -> CodeConverter (V.Vector Raw.LineNumberTableEntry)
        convertLineNumberTable = fmap V.fromList . traverse convertLineNumberTableEntry

        convertLineNumberTableEntry :: Abs.LineNumberTableEntry -> CodeConverter Raw.LineNumberTableEntry
        convertLineNumberTableEntry (Abs.LineNumberTableEntry a b) = pure $ Raw.LineNumberTableEntry a b
    convertCodeAttribute' (StackMapTable frames) = do
        frames' <- convertStackMapTable frames
        nameIndex <- findIndexOf (CPUTF8Entry "StackMapTable")
        pure $ Raw.AttributeInfo (fromIntegral nameIndex) (Raw.StackMapTableAttribute frames')
      where
        convertStackMapTable :: [Abs.StackMapFrame] -> CodeConverter (V.Vector Raw.StackMapFrame)
        convertStackMapTable = fmap (V.fromList . snd) . foldMWith convertStackMapFrame -1

        convertStackMapFrame :: U2 -> Abs.StackMapFrame -> CodeConverter (U2, Raw.StackMapFrame)
        convertStackMapFrame prev (Abs.SameFrame x) = do
            label <- (- 1) . (- prev) <$> fullyResolveAbs x
            pure
                ( label
                , if label <= 63
                    then Raw.SameFrame (fromIntegral label)
                    else
                        if label <= 32767
                            then Raw.SameFrameExtended label
                            else error "Label too large"
                )
        convertStackMapFrame prev (Abs.ChopFrame x stack) = do
            label <- (- 1) . (- prev) <$> fullyResolveAbs stack
            pure
                ( label
                , Raw.ChopFrame x (fromIntegral label)
                )
        convertStackMapFrame prev (Abs.SameLocals1StackItemFrame x stack) = do
            label <- (- 1) . (- prev) <$> fullyResolveAbs stack
            x' <- convertVerificationTypeInfo x
            pure
                ( label
                , if label <= 63
                    then Raw.SameLocals1StackItemFrame x' (fromIntegral label)
                    else
                        if label <= 32767
                            then Raw.SameLocals1StackItemFrameExtended x' label
                            else error "Label too large"
                )
        convertStackMapFrame prev (Abs.AppendFrame x stack) = do
            label <- (- 1) . (- prev) <$> fullyResolveAbs stack
            x' <- traverse convertVerificationTypeInfo x
            pure
                ( label
                , if label <= 32767
                    then Raw.AppendFrame (V.fromList x') label
                    else error "Label too large"
                )
        convertStackMapFrame prev (Abs.FullFrame x y stack) = do
            label <- (- 1) . (- prev) <$> fullyResolveAbs stack
            x' <- traverse convertVerificationTypeInfo x
            y' <- traverse convertVerificationTypeInfo y
            pure
                ( label
                , if label <= 32767
                    then Raw.FullFrame (V.fromList x') (V.fromList y') label
                    else error "Label too large"
                )

convertVerificationTypeInfo :: Abs.VerificationTypeInfo -> CodeConverter Raw.VerificationTypeInfo
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

convertMethod :: HasCallStack => Abs.ClassFileMethod -> ConvertM Raw.MethodInfo
convertMethod Abs.ClassFileMethod{..} = do
    let flags = accessFlagsToWord16 methodAccessFlags
    nameIndex <- findIndexOf (CPUTF8Entry methodName)
    descriptorIndex <- findIndexOf (CPUTF8Entry (convertMethodDescriptor methodDescriptor))
    attributes <- traverse convertMethodAttribute (TML.toVector methodAttributes)
    pure $ Raw.MethodInfo flags (fromIntegral nameIndex) (fromIntegral descriptorIndex) attributes
