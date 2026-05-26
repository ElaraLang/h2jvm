{-# LANGUAGE LambdaCase #-}

-- | Conversion of access flags from the high level ADT to low level bitfield representations.
module H2JVM.Internal.Convert.AccessFlag (accessFlagsToWord16) where

import Data.Bits ((.|.))
import Data.Word (Word16)

import H2JVM.ClassFile.AccessFlags (ClassAccessFlag (..), FieldAccessFlag (..), MethodAccessFlag (..))
import H2JVM.Internal.Raw.AccessFlags (accessFlagValue)

import H2JVM.Internal.Raw.AccessFlags qualified as Raw

-- | A convertible access flag
class ConvertAccessFlag a where
    convertAccessFlag :: a -> Raw.AccessFlag

{- | Convert some list of access flags to a bitmask of their low level representations.
>>> accessFlagsToWord16 @ClassAccessFlag []
0
-}
accessFlagsToWord16 :: ConvertAccessFlag a => [a] -> Word16
accessFlagsToWord16 = foldr (\flag acc -> acc .|. accessFlagValue (convertAccessFlag flag)) 0

instance ConvertAccessFlag ClassAccessFlag where
    convertAccessFlag = \case
        CPublic -> Raw.ACC_PUBLIC
        CFinal -> Raw.ACC_FINAL
        CSuper -> Raw.ACC_SUPER
        CInterface -> Raw.ACC_INTERFACE
        CAbstract -> Raw.ACC_ABSTRACT
        CSynthetic -> Raw.ACC_SYNTHETIC
        CAnnotation -> Raw.ACC_ANNOTATION
        CEnum -> Raw.ACC_ENUM

instance ConvertAccessFlag FieldAccessFlag where
    convertAccessFlag = \case
        FPublic -> Raw.ACC_PUBLIC
        FPrivate -> Raw.ACC_PRIVATE
        FProtected -> Raw.ACC_PROTECTED
        FStatic -> Raw.ACC_STATIC
        FFinal -> Raw.ACC_FINAL
        FVolatile -> Raw.ACC_VOLATILE
        FTransient -> Raw.ACC_TRANSIENT
        FSynthetic -> Raw.ACC_SYNTHETIC
        FEnum -> Raw.ACC_ENUM

instance ConvertAccessFlag MethodAccessFlag where
    convertAccessFlag = \case
        MPublic -> Raw.ACC_PUBLIC
        MPrivate -> Raw.ACC_PRIVATE
        MProtected -> Raw.ACC_PROTECTED
        MStatic -> Raw.ACC_STATIC
        MFinal -> Raw.ACC_FINAL
        MSynchronized -> Raw.ACC_SYNCHRONIZED
        MBridge -> Raw.ACC_BRIDGE
        MVarargs -> Raw.ACC_VARARGS
        MNative -> Raw.ACC_NATIVE
        MAbstract -> Raw.ACC_ABSTRACT
        MStrict -> Raw.ACC_STRICT
        MSynthetic -> Raw.ACC_SYNTHETIC
