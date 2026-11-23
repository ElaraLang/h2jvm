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
    {- | A symmetric relation of access flag compatibility.
    This resolves conflicts between access flags of the same type, e.g. public and private at once.
    -}
    isCompatibleWith :: a -> a -> Bool

    convertAccessFlag :: a -> Raw.AccessFlag

{- | Convert some list of access flags to a bitmask of their low level representations.
Filters out incompatible flags with a bias toward the earlier flag in the list, e.g. [@public@, @private@] => [@public@].
(This ordering might seem odd, but consider that in e.g. 'H2JVM.Builder.ClassBuilder', 'H2JVM.Builder.addAccessFlag' internally *prepends*, so an "earlier" entry is in fact a "later" overriding one.)
>>> accessFlagsToWord16 @ClassAccessFlag []
0
>>> accessFlagsToWord16 @ClassAccessFlag [CPublic, CFinal]
17
>>> accessFlagsToWord16 [FPublic, FPrivate] == accessFlagsToWord16 [FPublic]
True
-}
accessFlagsToWord16 :: ConvertAccessFlag a => [a] -> Word16
accessFlagsToWord16 = toBitmask . sanitise []
  where
    toBitmask = foldr (\f acc -> acc .|. accessFlagValue (convertAccessFlag f)) 0
    sanitise _accepted [] = []
    sanitise accepted (f : fs)
        | not (all (isCompatibleWith f) accepted) = sanitise accepted fs
        | otherwise = f : sanitise (f : accepted) fs

-- | Build a symmetric compatibility relation from a list of mutually exclusive pairs.
compatibleExcept :: Eq a => [(a, a)] -> a -> a -> Bool
compatibleExcept conflicts a b = (a, b) `notElem` conflicts && (b, a) `notElem` conflicts

instance ConvertAccessFlag ClassAccessFlag where
    isCompatibleWith = compatibleExcept [(CInterface, CEnum), (CFinal, CAbstract)]

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
    isCompatibleWith =
        compatibleExcept
            [ (FPublic, FPrivate)
            , (FPublic, FProtected)
            , (FPrivate, FProtected)
            , (FFinal, FVolatile)
            ]

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

    isCompatibleWith =
        compatibleExcept
            ( [(MPublic, MPrivate), (MPublic, MProtected), (MPrivate, MProtected)]
                <> [(MAbstract, f) | f <- [MPrivate, MStatic, MFinal, MSynchronized, MNative, MStrict]]
            )
