{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

-- | Converts between high level and low level representations
module JVM.Data.Convert where

import Data.IndexedMap qualified as IM
import Data.Maybe (fromMaybe)
import Data.TypeMergingList qualified as TML
import Data.Vector qualified as V
import JVM.Data.Abstract.ClassFile qualified as Abs
import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (CPClassEntry, CPUTF8Entry))
import JVM.Data.Abstract.Name (QualifiedClassName, parseQualifiedClassName)
import JVM.Data.Abstract.Type (ClassInfoType (..))
import JVM.Data.Convert.AccessFlag (accessFlagsToWord16)
import JVM.Data.Convert.ConstantPool
import JVM.Data.Convert.Field (convertField)
import JVM.Data.Convert.Method (convertMethod)
import JVM.Data.Convert.Monad
import JVM.Data.JVMVersion (getMajor, getMinor)
import JVM.Data.Raw.ClassFile (Attribute (BootstrapMethodsAttribute))
import JVM.Data.Raw.ClassFile qualified as Raw
import JVM.Data.Raw.MagicNumbers qualified as MagicNumbers
import Polysemy

jloName :: QualifiedClassName
jloName = parseQualifiedClassName "java.lang.Object"

convertClassAttributes :: (ConvertEff r) => [Abs.ClassFileAttribute] -> Sem r [Raw.AttributeInfo]
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
    convertClassAttribute other = error $ "Unsupported class attribute: " <> show other

    convertInnerClass Abs.InnerClassInfo{..} = do
        innerIndex <- findIndexOf (CPClassEntry $ ClassInfoType innerClassInfo)
        outerIndex <- (findIndexOf . CPClassEntry . ClassInfoType) outerClassInfo
        nameIndex <- (findIndexOf . CPUTF8Entry) innerName
        let innerFlags = accessFlagsToWord16 accessFlags
        pure $ Raw.InnerClassInfo innerIndex outerIndex nameIndex innerFlags

convert :: Abs.ClassFile -> Either CodeConverterError Raw.ClassFile
convert Abs.ClassFile{..} = do
    (tempClass, cpState) <- run $ runConvertM $ do
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

    let (bmIndex, finalConstantPool) = run $ runConstantPoolWith cpState $ do
            let bootstrapAttr = BootstrapMethodsAttribute (IM.toVector cpState.bootstrapMethods)
            attrNameIndex <- findIndexOf (CPUTF8Entry "BootstrapMethods")
            pure $ Raw.AttributeInfo attrNameIndex bootstrapAttr

    pure $ tempClass{Raw.constantPool = IM.toVector finalConstantPool.constantPool, Raw.attributes = bmIndex `V.cons` tempClass.attributes}
