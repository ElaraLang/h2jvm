{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Converts between high level and low level representations
module JVM.Data.Convert where

import Control.Monad.State
import Data.Bits ((.|.))
import Data.IndexedMap (IndexedMap)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Word (Word16)
import JVM.Data.Abstract.AccessFlags qualified as AbsFlag
import JVM.Data.Abstract.ClassFile qualified as Abs
import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (CPClassEntry, CPUTF8Entry), findIndexOf, runConstantPoolM)
import JVM.Data.Abstract.Name (QualifiedClassName, parseQualifiedClassName, toInternalName)
import JVM.Data.Convert.Field (convertField)
import JVM.Data.JVMVersion (getMajor, getMinor)
import JVM.Data.Raw.AccessFlags
import JVM.Data.Raw.ClassFile qualified as Raw
import JVM.Data.Raw.ConstantPool (ConstantPoolInfo)
import JVM.Data.Raw.MagicNumbers qualified as MagicNumbers

jloName :: QualifiedClassName
jloName = parseQualifiedClassName "java.lang.Object"

convertAccessFlags :: [AbsFlag.ClassAccessFlag] -> Word16
convertAccessFlags = foldr ((.|.) . convertAccessFlag) 0
  where
    convertAccessFlag =
        accessFlagValue . \case
            AbsFlag.Public -> ACC_PUBLIC
            AbsFlag.Final -> ACC_FINAL
            AbsFlag.Super -> ACC_SUPER
            AbsFlag.Interface -> ACC_INTERFACE
            AbsFlag.Abstract -> ACC_ABSTRACT
            AbsFlag.Synthetic -> ACC_SYNTHETIC
            AbsFlag.Annotation -> ACC_ANNOTATION
            AbsFlag.Enum -> ACC_ENUM

convertClassAttributes :: [Abs.ClassFileAttribute] -> State (IndexedMap ConstantPoolInfo) [Raw.AttributeInfo]
convertClassAttributes = traverse convertClassAttribute
  where
    convertClassAttribute (Abs.SourceFile text) = do
        nameIndex <- findIndexOf (CPUTF8Entry "SourceFile")
        textIndex <- findIndexOf (CPUTF8Entry text)
        pure $ Raw.AttributeInfo nameIndex (Raw.SourceFileAttribute textIndex)
    convertClassAttribute _ = error "convertClassAttribute"

convert :: Abs.ClassFile -> Raw.ClassFile
convert Abs.ClassFile{..} = do
    let (temp, finalConstantPool) = runConstantPoolM $ do
            nameIndex <- findIndexOf (CPClassEntry $ toInternalName name)
            superIndex <- findIndexOf (CPClassEntry $ toInternalName (fromMaybe jloName superClass))
            let flags = convertAccessFlags accessFlags
            interfaces' <- traverse (findIndexOf . CPClassEntry . toInternalName) interfaces
            attributes' <- convertClassAttributes attributes
            fields' <- traverse convertField fields
            pure $
                Raw.ClassFile
                    MagicNumbers.classMagic
                    (getMinor version)
                    (getMajor version)
                    mempty -- temporary empty constant pool
                    flags
                    nameIndex
                    superIndex
                    (V.fromList interfaces')
                    (V.fromList fields')
                    mempty
                    (V.fromList attributes')
    temp{Raw.constantPool = finalConstantPool}
