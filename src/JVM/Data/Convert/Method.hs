{-# LANGUAGE RecordWildCards #-}

module JVM.Data.Convert.Method where

import Data.Vector qualified as V
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.ClassFile.Method qualified as Abs
import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (CPUTF8Entry))
import JVM.Data.Convert.AccessFlag (accessFlagsToWord16)
import JVM.Data.Convert.ConstantPool
import JVM.Data.Convert.Descriptor (convertMethodDescriptor)
import JVM.Data.Convert.Instruction (convertInstructions, fullyRunCodeConverter)
import JVM.Data.Convert.Monad (ConvertM)
import JVM.Data.Raw.ClassFile qualified as Raw

convertMethodAttribute :: Abs.MethodAttribute -> ConvertM Raw.AttributeInfo
convertMethodAttribute (Abs.Code (Abs.CodeAttributeData{..})) = do
    code' <- fmap V.fromList $ fullyRunCodeConverter $ convertInstructions code
    exceptionTable' <- convertExceptionTable exceptionTable
    attributes' <- convertCodeAttributes codeAttributes
    nameIndex <- findIndexOf (CPUTF8Entry "Code")

    pure $ Raw.AttributeInfo (fromIntegral nameIndex) (Raw.CodeAttribute maxStack maxLocals code' exceptionTable' attributes')
  where
    convertExceptionTable :: [Abs.ExceptionTableEntry] -> ConvertM (V.Vector Raw.ExceptionTableEntry)
    convertExceptionTable = fmap V.fromList . traverse convertExceptionTableEntry

    convertExceptionTableEntry :: Abs.ExceptionTableEntry -> ConvertM Raw.ExceptionTableEntry
    convertExceptionTableEntry = undefined

    convertCodeAttributes :: [Abs.CodeAttribute] -> ConvertM (V.Vector Raw.AttributeInfo)
    convertCodeAttributes = fmap V.fromList . traverse convertCodeAttribute'

    convertCodeAttribute' :: Abs.CodeAttribute -> ConvertM Raw.AttributeInfo
    convertCodeAttribute' (LineNumberTable lns) = do
        lns' <- convertLineNumberTable lns
        nameIndex <- findIndexOf (CPUTF8Entry "LineNumberTable")
        pure $ Raw.AttributeInfo (fromIntegral nameIndex) (Raw.LineNumberTableAttribute lns')
      where
        convertLineNumberTable :: [Abs.LineNumberTableEntry] -> ConvertM (V.Vector Raw.LineNumberTableEntry)
        convertLineNumberTable = fmap V.fromList . traverse convertLineNumberTableEntry

        convertLineNumberTableEntry :: Abs.LineNumberTableEntry -> ConvertM Raw.LineNumberTableEntry
        convertLineNumberTableEntry (Abs.LineNumberTableEntry a b) = pure $ Raw.LineNumberTableEntry a b
    convertCodeAttribute' (StackMapTable frames) = do
        frames' <- convertStackMapTable frames
        nameIndex <- findIndexOf (CPUTF8Entry "StackMapTable")
        pure $ Raw.AttributeInfo (fromIntegral nameIndex) (Raw.StackMapTableAttribute frames')
      where
        convertStackMapTable :: [Abs.StackMapFrame] -> ConvertM (V.Vector Raw.StackMapFrame)
        convertStackMapTable = fmap V.fromList . traverse convertStackMapFrame

        convertStackMapFrame :: Abs.StackMapFrame -> ConvertM Raw.StackMapFrame
        convertStackMapFrame Abs.SameFrame = pure Raw.SameFrame

convertMethod :: Abs.ClassFileMethod -> ConvertM Raw.MethodInfo
convertMethod Abs.ClassFileMethod{..} = do
    let flags = accessFlagsToWord16 methodAccessFlags
    nameIndex <- findIndexOf (CPUTF8Entry methodName)
    descriptorIndex <- findIndexOf (CPUTF8Entry (convertMethodDescriptor methodDescriptor))
    attributes <- traverse convertMethodAttribute methodAttributes
    pure $ Raw.MethodInfo flags (fromIntegral nameIndex) (fromIntegral descriptorIndex) (V.fromList attributes)
