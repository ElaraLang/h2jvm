module JVM.Data.Abstract.ClassFile.AccessFlags where

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
    deriving (Show, Eq)

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
    deriving (Show)

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
    deriving (Show)
