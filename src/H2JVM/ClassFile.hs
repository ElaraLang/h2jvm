{-# LANGUAGE DuplicateRecordFields #-}

-- | High level representation of a class file
module H2JVM.ClassFile (ClassFile (..), ClassFileAttribute (..), InnerClassInfo (..)) where

import Data.Data
import Data.Text (Text)
import Prettyprinter (encloseSep, line, parens)

import H2JVM.ClassFile.AccessFlags (ClassAccessFlag)
import H2JVM.ClassFile.Field (ClassFileField)
import H2JVM.ClassFile.Method (ClassFileMethod)
import H2JVM.ConstantPool (BootstrapMethod)
import H2JVM.Data.TypeMergingList (DataMergeable (merge), TypeMergingList, errorDifferentConstructors, toList)
import H2JVM.Internal.Pretty
import H2JVM.JVMVersion (JVMVersion)
import H2JVM.Name

data ClassFile = ClassFile
    { name :: QualifiedClassName
    , version :: JVMVersion
    , accessFlags :: [ClassAccessFlag]
    , superClass :: Maybe QualifiedClassName
    , interfaces :: [QualifiedClassName]
    , fields :: [ClassFileField]
    , methods :: [ClassFileMethod]
    , attributes :: TypeMergingList ClassFileAttribute
    }
    deriving (Show)

data ClassFileAttribute
    = InnerClasses [InnerClassInfo]
    | EnclosingMethod
    | Synthetic
    | Signature
    | SourceFile Text
    | SourceDebugExtension
    | Deprecated
    | RuntimeVisibleAnnotations
    | RuntimeInvisibleAnnotations
    | BootstrapMethods [BootstrapMethod]
    deriving (Data, Eq, Show)

instance DataMergeable ClassFileAttribute where
    merge (InnerClasses a) (InnerClasses b) = InnerClasses (a <> b)
    merge EnclosingMethod EnclosingMethod = EnclosingMethod
    merge Synthetic Synthetic = Synthetic
    merge Signature Signature = Signature
    merge (SourceFile a) (SourceFile b) = SourceFile (a <> b)
    merge SourceDebugExtension SourceDebugExtension = SourceDebugExtension
    merge Deprecated Deprecated = Deprecated
    merge RuntimeVisibleAnnotations RuntimeVisibleAnnotations = RuntimeVisibleAnnotations
    merge RuntimeInvisibleAnnotations RuntimeInvisibleAnnotations = RuntimeInvisibleAnnotations
    merge (BootstrapMethods a) (BootstrapMethods b) = BootstrapMethods (a <> b)
    merge a b = errorDifferentConstructors a b

data InnerClassInfo = InnerClassInfo
    { innerClassInfo :: QualifiedClassName
    , outerClassInfo :: QualifiedClassName
    , innerName :: Text
    , accessFlags :: [ClassAccessFlag]
    }
    deriving (Data, Eq, Show)

instance Pretty ClassFile where
    pretty ClassFile{name, version, accessFlags, superClass, interfaces, fields, methods, attributes} =
        hsep (pretty <$> accessFlags)
            <+> "class"
            <+> pretty name
            <+> parens (pretty version)
            <+> maybe "" (("extends" <+>) . pretty) superClass
            <+> (if null interfaces then "" else "implements" <+> pretty interfaces)
            <+> body
      where
        body = encloseSep open close sep (map pretty fields ++ map pretty methods ++ map pretty (toList attributes))
        open = "{"
        close = "}"
        sep = line

instance Pretty ClassFileAttribute where
    pretty (InnerClasses xs) = "InnerClasses" <+> pretty xs
    pretty EnclosingMethod = "EnclosingMethod"
    pretty Synthetic = "Synthetic"
    pretty Signature = "Signature"
    pretty (SourceFile x) = "SourceFile" <+> pretty x
    pretty SourceDebugExtension = "SourceDebugExtension"
    pretty Deprecated = "Deprecated"
    pretty RuntimeVisibleAnnotations = "RuntimeVisibleAnnotations"
    pretty RuntimeInvisibleAnnotations = "RuntimeInvisibleAnnotations"
    pretty (BootstrapMethods xs) = "BootstrapMethods" <+> pretty xs

instance Pretty InnerClassInfo where
    pretty InnerClassInfo{innerClassInfo, outerClassInfo, innerName, accessFlags} =
        pretty accessFlags <+> "inner class" <+> pretty innerClassInfo <+> "in" <+> pretty outerClassInfo <+> "named" <+> pretty innerName
