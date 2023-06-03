{-# LANGUAGE LambdaCase #-}

module JVM.Data.Convert.AccessFlag where

import Data.Bits ((.|.))
import Data.Word (Word16)
import JVM.Data.Abstract.AccessFlags (ClassAccessFlag (..), FieldAccessFlag (..), MethodAccessFlag (..))
import JVM.Data.Raw.AccessFlags (accessFlagValue)
import JVM.Data.Raw.AccessFlags qualified as Raw

class ConvertAccessFlag a where
    convertAccessFlag :: a -> Raw.AccessFlag

accessFlagsToWord16 :: ConvertAccessFlag a => [a] -> Word16
accessFlagsToWord16 = foldr (\flag acc -> acc .|. accessFlagValue (convertAccessFlag flag)) 0

instance ConvertAccessFlag ClassAccessFlag where
    convertAccessFlag = \case
        Public -> Raw.ACC_PUBLIC
        Final -> Raw.ACC_FINAL
        Super -> Raw.ACC_SUPER
        Interface -> Raw.ACC_INTERFACE
        Abstract -> Raw.ACC_ABSTRACT
        Synthetic -> Raw.ACC_SYNTHETIC
        Annotation -> Raw.ACC_ANNOTATION
        Enum -> Raw.ACC_ENUM

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
