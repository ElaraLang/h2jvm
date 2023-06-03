{-# LANGUAGE RecordWildCards #-}

module JVM.Data.Convert.Method where

import Data.Text (Text)
import Data.Vector qualified as V
import JVM.Data.Abstract.ClassFile (ClassFile (name))
import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (CPUTF8Entry), ConstantPoolM, findIndexOf)
import JVM.Data.Abstract.Method
import JVM.Data.Abstract.Method qualified as Abs
import JVM.Data.Convert.AccessFlag (accessFlagsToWord16)
import JVM.Data.Convert.Type (fieldTypeDescriptor)
import JVM.Data.Raw.ClassFile qualified as Raw

convertMethodDescriptor :: Abs.MethodDescriptor -> Text
convertMethodDescriptor (Abs.MethodDescriptor params ret) =
    let params' = map fieldTypeDescriptor params
        ret' = case ret of
            Abs.VoidReturn -> "V"
            Abs.Return t -> fieldTypeDescriptor t
     in "(" <> mconcat params' <> ")" <> ret'

convertMethodAttribute :: Abs.MethodAttribute -> ConstantPoolM Raw.AttributeInfo
convertMethodAttribute (Abs.Code (Abs.CodeAttributeData{..})) = do
    let maxStack' = fromIntegral maxStack
        maxLocals' = fromIntegral maxLocals
        code' = V.fromList code

    exceptionTable' <- convertExceptionTable exceptionTable
    attributes' <- convertCodeAttributes codeAttributes
    nameIndex <- findIndexOf (CPUTF8Entry "Code")

    pure $ Raw.AttributeInfo nameIndex (Raw.CodeAttribute maxStack' maxLocals' code' exceptionTable' attributes')
  where
    convertExceptionTable :: [Abs.ExceptionTableEntry] -> ConstantPoolM (V.Vector Raw.ExceptionTableEntry)
    convertExceptionTable = fmap V.fromList . traverse convertExceptionTableEntry

    convertExceptionTableEntry :: Abs.ExceptionTableEntry -> ConstantPoolM Raw.ExceptionTableEntry
    convertExceptionTableEntry = undefined

    convertCodeAttributes :: [Abs.CodeAttribute] -> ConstantPoolM (V.Vector Raw.AttributeInfo)
    convertCodeAttributes = fmap V.fromList . traverse convertCodeAttribute'

    convertCodeAttribute' :: Abs.CodeAttribute -> ConstantPoolM Raw.AttributeInfo
    convertCodeAttribute' = undefined

convertMethod :: Abs.ClassFileMethod -> ConstantPoolM Raw.MethodInfo
convertMethod Abs.ClassFileMethod{..} = do
    let flags = accessFlagsToWord16 methodAccessFlags
    nameIndex <- findIndexOf (CPUTF8Entry methodName)
    descriptorIndex <- findIndexOf (CPUTF8Entry (convertMethodDescriptor methodDescriptor))
    attributes <- traverse convertMethodAttribute methodAttributes
    pure $ Raw.MethodInfo flags nameIndex descriptorIndex (V.fromList attributes)
