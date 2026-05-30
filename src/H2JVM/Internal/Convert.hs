{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

-- | Converts between high level and low level representations
module H2JVM.Internal.Convert (jloName, convert) where

import Data.Maybe (fromMaybe)
import Effectful
import Effectful.Error.Static

import Data.Vector qualified as V

import H2JVM.ConstantPool (ConstantPoolEntry (CPClassEntry, CPUTF8Entry))
import H2JVM.Internal.Convert.AccessFlag (accessFlagsToWord16)
import H2JVM.Internal.Convert.ConstantPool
import H2JVM.Internal.Convert.Field (convertField)
import H2JVM.Internal.Convert.Method (convertMethod)
import H2JVM.Internal.Convert.Monad
import H2JVM.Internal.Raw.ClassFile (Attribute (BootstrapMethodsAttribute))
import H2JVM.JVMVersion (getMajor, getMinor)
import H2JVM.Name (QualifiedClassName, parseQualifiedClassName)
import H2JVM.Type (ClassInfoType (..))

import H2JVM.ClassFile qualified as Abs
import H2JVM.Data.TypeMergingList qualified as TML
import H2JVM.Internal.IndexedMap qualified as IM
import H2JVM.Internal.Raw.ClassFile qualified as Raw
import H2JVM.Internal.Raw.MagicNumbers qualified as MagicNumbers

jloName :: QualifiedClassName
jloName = parseQualifiedClassName "java.lang.Object"

convertClassAttributes :: ConvertEff r => [Abs.ClassFileAttribute] -> Eff r [Raw.AttributeInfo]
convertClassAttributes = traverse convertClassAttribute
  where
    convertClassAttribute (Abs.SourceFile text) = do
        nameIndex <- findIndexOf (CPUTF8Entry "SourceFile")
        textIndex <- findIndexOf (CPUTF8Entry text)
        pure $ Raw.AttributeInfo nameIndex (Raw.SourceFileAttribute textIndex)
    convertClassAttribute (Abs.InnerClasses classes) = do
        nameIndex <- findIndexOf (CPUTF8Entry "InnerClasses")
        classes' <- traverse convertInnerClass classes
        pure $ Raw.AttributeInfo nameIndex (Raw.InnerClassesAttribute (V.fromList classes'))
    convertClassAttribute other = throwError $ UnsupportedClassAttribute (show other)

    convertInnerClass Abs.InnerClassInfo{..} = do
        innerIndex <- findIndexOf (CPClassEntry $ ClassInfoType innerClassInfo)
        outerIndex <- (findIndexOf . CPClassEntry . ClassInfoType) outerClassInfo
        nameIndex <- (findIndexOf . CPUTF8Entry) innerName
        let innerFlags = accessFlagsToWord16 accessFlags
        pure $ Raw.InnerClassInfo innerIndex outerIndex nameIndex innerFlags

convert :: Abs.ClassFile -> Either CodeConverterError Raw.ClassFile
convert Abs.ClassFile{..} = runPureEff $ runErrorNoCallStack $ do
    (tempClass, cpState) <- runConstantPool $ do
        nameIndex <- findIndexOf (CPClassEntry $ ClassInfoType name)
        superIndex <- findIndexOf (CPClassEntry $ ClassInfoType (fromMaybe jloName superClass))
        let flags = accessFlagsToWord16 accessFlags
        interfaces' <- traverse (findIndexOf . CPClassEntry . ClassInfoType) interfaces
        attributes' <- convertClassAttributes (TML.toList attributes)
        fields' <- traverse convertField fields
        methods' <- traverse convertMethod methods

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
                (V.fromList methods')
                (V.fromList attributes')

    (bmIndex, finalConstantPool) <- runConstantPoolWith cpState $ do
        let bootstrapAttr = BootstrapMethodsAttribute (IM.toVector cpState.bootstrapMethods)
        attrNameIndex <- findIndexOf (CPUTF8Entry "BootstrapMethods")
        pure $ Raw.AttributeInfo attrNameIndex bootstrapAttr

    pure $ tempClass{Raw.constantPool = IM.toVector finalConstantPool.constantPool, Raw.attributes = bmIndex `V.cons` tempClass.attributes}
