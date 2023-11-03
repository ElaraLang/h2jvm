module JVM.Data.Abstract.ClassFile.AccessFlags where

import Data.Data

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
