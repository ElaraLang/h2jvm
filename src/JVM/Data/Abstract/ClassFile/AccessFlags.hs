module JVM.Data.Abstract.ClassFile.AccessFlags where

import Data.Data
import JVM.Data.Pretty (Pretty (pretty))

-- | Access flags for a class
data ClassAccessFlag
    = Public
    | Final
    | Super
    | Interface
    | Abstract
    | Synthetic
    | Annotation
    | Enum
    deriving (Show, Eq, Data)

data FieldAccessFlag
    = FPublic
    | FPrivate
    | FProtected
    | FStatic
    | FFinal
    | FVolatile
    | FTransient
    | FSynthetic
    | FEnum
    deriving (Show, Data)

data MethodAccessFlag
    = MPublic
    | MPrivate
    | MProtected
    | MStatic
    | MFinal
    | MSynchronized
    | MBridge
    | MVarargs
    | MNative
    | MAbstract
    | MStrict
    | MSynthetic
    deriving (Show, Data)

instance Pretty ClassAccessFlag where
    pretty Public = "public"
    pretty Final = "final"
    pretty Super = "super"
    pretty Interface = "interface"
    pretty Abstract = "abstract"
    pretty Synthetic = "synthetic"
    pretty Annotation = "@interface"
    pretty Enum = "enum"

instance Pretty FieldAccessFlag where
    pretty FPublic = "public"
    pretty FPrivate = "private"
    pretty FProtected = "protected"
    pretty FStatic = "static"
    pretty FFinal = "final"
    pretty FVolatile = "volatile"
    pretty FTransient = "transient"
    pretty FSynthetic = "synthetic"
    pretty FEnum = "enum"

instance Pretty MethodAccessFlag where
    pretty MPublic = "public"
    pretty MPrivate = "private"
    pretty MProtected = "protected"
    pretty MStatic = "static"
    pretty MFinal = "final"
    pretty MSynchronized = "synchronized"
    pretty MBridge = "bridge"
    pretty MVarargs = "varargs"
    pretty MNative = "native"
    pretty MAbstract = "abstract"
    pretty MStrict = "strict"
    pretty MSynthetic = "synthetic"