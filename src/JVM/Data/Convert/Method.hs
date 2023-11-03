{-# LANGUAGE RecordWildCards #-}

module JVM.Data.Convert.Method where

import Control.Applicative (liftA2)
import Data.Vector qualified as V
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.ClassFile.Method qualified as Abs
import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (CPUTF8Entry))
import JVM.Data.Convert.AccessFlag (accessFlagsToWord16)
import JVM.Data.Convert.ConstantPool
import JVM.Data.Convert.Descriptor (convertMethodDescriptor)
import JVM.Data.Convert.Instruction (CodeConverter, convertInstructions, fullyResolveAbs, fullyRunCodeConverter)
import JVM.Data.Convert.Monad (ConvertM)
import JVM.Data.Raw.ClassFile qualified as Raw

convertMethodAttribute :: Abs.MethodAttribute -> ConvertM Raw.AttributeInfo
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
        convertStackMapTable = fmap V.fromList . traverse convertStackMapFrame

        convertStackMapFrame :: Abs.StackMapFrame -> CodeConverter Raw.StackMapFrame
        convertStackMapFrame (Abs.SameFrame x) = do
            label <- fullyResolveAbs x
            pure $
                if label <= 63
                    then Raw.SameFrame (fromIntegral label)
                    else
                        if label <= 32767
                            then Raw.SameFrameExtended label
                            else error "Label too large"

convertMethod :: Abs.ClassFileMethod -> ConvertM Raw.MethodInfo
convertMethod Abs.ClassFileMethod{..} = do
    let flags = accessFlagsToWord16 methodAccessFlags
    nameIndex <- findIndexOf (CPUTF8Entry methodName)
    descriptorIndex <- findIndexOf (CPUTF8Entry (convertMethodDescriptor methodDescriptor))
    attributes <- traverse convertMethodAttribute methodAttributes
    pure $ Raw.MethodInfo flags (fromIntegral nameIndex) (fromIntegral descriptorIndex) (V.fromList attributes)
