module JVM.Data.Abstract.AccessFlags where

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
    deriving (Show)

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