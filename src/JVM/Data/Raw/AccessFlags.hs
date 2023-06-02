module JVM.Data.Raw.AccessFlags where

import Data.Binary (Word16)
import Data.Binary.Put (putWord16be)
import Data.Binary.Write (WriteBinary (writeBinary))
import JVM.Data.Raw.MagicNumbers qualified as MagicNumbers

data AccessFlags
    = ACC_PUBLIC
    | ACC_PRIVATE
    | ACC_PROTECTED
    | ACC_STATIC
    | ACC_FINAL
    | ACC_VOLATILE
    | ACC_TRANSIENT
    | ACC_SUPER
    | ACC_INTERFACE
    | ACC_ABSTRACT
    | ACC_SYNTHETIC
    | ACC_ANNOTATION
    | ACC_ENUM
    deriving (Show)

accessFlagValue :: AccessFlags -> Word16
accessFlagValue ACC_PUBLIC = MagicNumbers.accessFlag_PUBLIC
accessFlagValue ACC_PRIVATE = MagicNumbers.accessFlag_PRIVATE
accessFlagValue ACC_PROTECTED = MagicNumbers.accessFlag_PROTECTED
accessFlagValue ACC_STATIC = MagicNumbers.accessFlag_STATIC
accessFlagValue ACC_FINAL = MagicNumbers.accessFlag_FINAL
accessFlagValue ACC_VOLATILE = MagicNumbers.accessFlag_VOLATILE
accessFlagValue ACC_TRANSIENT = MagicNumbers.accessFlag_TRANSIENT
accessFlagValue ACC_SUPER = MagicNumbers.accessFlag_SUPER
accessFlagValue ACC_INTERFACE = MagicNumbers.accessFlag_INTERFACE
accessFlagValue ACC_ABSTRACT = MagicNumbers.accessFlag_ABSTRACT
accessFlagValue ACC_SYNTHETIC = MagicNumbers.accessFlag_SYNTHETIC
accessFlagValue ACC_ANNOTATION = MagicNumbers.accessFlag_ANNOTATION
accessFlagValue ACC_ENUM = MagicNumbers.accessFlag_ENUM

instance WriteBinary AccessFlags where
    writeBinary = putWord16be . accessFlagValue
