{- | Access flags for classes, fields, and methods in Java class files.
This uses the JVM meaning of "access flag", which includes any kind of modifier, including things like @interface@ and @enum@ that aren't really "access" modifiers in the Java language.
-}
module H2JVM.ClassFile.AccessFlags (ClassAccessFlag (..), FieldAccessFlag (..), MethodAccessFlag (..)) where

import Data.Data

import H2JVM.Internal.Pretty (Pretty (pretty))

-- | Access flags for a class.
data ClassAccessFlag
    = CPublic
    | CFinal
    | CSuper
    | CInterface
    | CAbstract
    | CSynthetic
    | CAnnotation
    | CEnum
    deriving (Data, Eq, Show)

-- | Access flags for a field.
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
    deriving (Data, Show)

-- | Access flags for a method.
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
    deriving (Data, Eq, Show)

instance Pretty ClassAccessFlag where
    pretty CPublic = "public"
    pretty CFinal = "final"
    pretty CSuper = "super"
    pretty CInterface = "interface"
    pretty CAbstract = "abstract"
    pretty CSynthetic = "synthetic"
    pretty CAnnotation = "@interface"
    pretty CEnum = "enum"

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
