{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Converts between high level and low level representations
module JVM.Data.Convert where

import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Word (Word16)
import JVM.Data.Abstract.AccessFlags qualified as Abs
import JVM.Data.Abstract.ClassFile qualified as Abs
import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (CPClassEntry, CPUTF8Entry), findIndexOf, runConstantPoolM)
import JVM.Data.Abstract.JVMVersion (getMajor, getMinor)
import JVM.Data.Abstract.Name (QualifiedClassName, parseQualifiedClassName, toInternalName)
import JVM.Data.Raw.AccessFlags
import JVM.Data.Raw.ClassFile qualified as Raw
import JVM.Data.Raw.MagicNumbers qualified as MagicNumbers

jloName :: QualifiedClassName
jloName = parseQualifiedClassName "java.lang.Object"

convertAccessFlags :: [Abs.ClassAccessFlag] -> Word16
convertAccessFlags = foldr ((.|.) . convertAccessFlag) 0
  where
    convertAccessFlag =
        accessFlagValue . \case
            Abs.Public -> ACC_PUBLIC
            Abs.Final -> ACC_FINAL
            Abs.Super -> ACC_SUPER
            Abs.Interface -> ACC_INTERFACE
            Abs.Abstract -> ACC_ABSTRACT
            Abs.Synthetic -> ACC_SYNTHETIC
            Abs.Annotation -> ACC_ANNOTATION
            Abs.Enum -> ACC_ENUM

convert :: Abs.ClassFile -> Raw.ClassFile
convert Abs.ClassFile{..} = do
    let (temp, finalConstantPool) = runConstantPoolM $ do
            nameIndex <- findIndexOf (CPClassEntry $ toInternalName name)
            superIndex <- findIndexOf (CPClassEntry $ toInternalName (fromMaybe jloName superClass))
            let flags = convertAccessFlags accessFlags
            interfaces' <- traverse (findIndexOf . CPClassEntry . toInternalName) interfaces
            pure $
                Raw.ClassFile
                    MagicNumbers.classMagic
                    (getMajor version)
                    (getMinor version)
                    mempty -- temporary empty constant pool
                    flags
                    nameIndex
                    superIndex
                    (V.fromList interfaces')
                    mempty
                    mempty
                    mempty
    (temp{Raw.constantPool = finalConstantPool})