-- | Information about JVM Versions.
module H2JVM.JVMVersion (
    JVMVersion,
    MajorVersion,
    MinorVersion,
    getMajor,
    getMinor,
    unwrapMajor,
    unwrapMinor,
    java6,
    java7,
    java8,
    java9,
    java10,
    java11,
    java12,
    java13,
    java14,
    java15,
    java16,
    java17,
    java18,
    java19,
    java20,
    java21,
    isLTS,
    isEOL,
)
where

import Data.Binary (Word16)

import H2JVM.Internal.Pretty

-- | A JVM Version Number.
newtype JVMVersion = JVMVersion Word16 deriving (Eq, Ord, Show)

-- | A Major JVM version.
newtype MajorVersion = MajorVersion Word16 deriving (Eq, Num, Ord, Show)

-- | A Minor JVM version.
newtype MinorVersion = MinorVersion Word16 deriving (Eq, Num, Ord, Show)

-- | Unwrap the 'MajorVersion' to obtain the raw 'Word16' value.
unwrapMajor :: MajorVersion -> Word16
unwrapMajor (MajorVersion v) = v

-- | Unwrap the 'MinorVersion' to obtain the raw 'Word16' value.
unwrapMinor :: MinorVersion -> Word16
unwrapMinor (MinorVersion v) = v

-- | Extract the 'MajorVersion' from a 'JVMVersion'.
getMajor :: JVMVersion -> MajorVersion
getMajor (JVMVersion v) = MajorVersion v

-- | Extract the 'MinorVersion' from a 'JVMVersion' (always 0 for standard JVMs).
getMinor :: JVMVersion -> MinorVersion
getMinor _ = MinorVersion 0 -- currently all JVM versions are 0

-- | JVM Version number representing Java 6 (class file major version 50).
java6 :: JVMVersion
java6 = JVMVersion 50

-- | JVM Version number representing Java 7 (class file major version 51).
java7 :: JVMVersion
java7 = JVMVersion 51

-- | JVM Version number representing Java 8 (class file major version 52).
java8 :: JVMVersion
java8 = JVMVersion 52

-- | JVM Version number representing Java 9 (class file major version 53).
java9 :: JVMVersion
java9 = JVMVersion 53

-- | JVM Version number representing Java 10 (class file major version 54).
java10 :: JVMVersion
java10 = JVMVersion 54

-- | JVM Version number representing Java 11 (class file major version 55).
java11 :: JVMVersion
java11 = JVMVersion 55

-- | JVM Version number representing Java 12 (class file major version 56).
java12 :: JVMVersion
java12 = JVMVersion 56

-- | JVM Version number representing Java 13 (class file major version 57).
java13 :: JVMVersion
java13 = JVMVersion 57

-- | JVM Version number representing Java 14 (class file major version 58).
java14 :: JVMVersion
java14 = JVMVersion 58

-- | JVM Version number representing Java 15 (class file major version 59).
java15 :: JVMVersion
java15 = JVMVersion 59

-- | JVM Version number representing Java 16 (class file major version 60).
java16 :: JVMVersion
java16 = JVMVersion 60

-- | JVM Version number representing Java 17 (class file major version 61).
java17 :: JVMVersion
java17 = JVMVersion 61

-- | JVM Version number representing Java 18 (class file major version 62).
java18 :: JVMVersion
java18 = JVMVersion 62

-- | JVM Version number representing Java 19 (class file major version 63).
java19 :: JVMVersion
java19 = JVMVersion 63

-- | JVM Version number representing Java 20 (class file major version 64).
java20 :: JVMVersion
java20 = JVMVersion 64

-- | JVM Version number representing Java 21 (class file major version 65).
java21 :: JVMVersion
java21 = JVMVersion 65

{- | Returns true if the JVM version is a Long-Term Support (LTS) release.

>>> isLTS java8
True

>>> isLTS java14
False
-}
isLTS :: JVMVersion -> Bool
isLTS jvm | jvm == java8 = True
isLTS jvm | jvm == java11 = True
isLTS jvm | jvm == java17 = True
isLTS jvm | jvm == java21 = True
isLTS _ = False

{- | Returns true if the JVM version is EOL at the time of writing (April 2023)
This is for OpenJDK, other vendors may have different EOL dates

>>> isEOL java8
False

>>> isEOL java6
True
-}
isEOL :: JVMVersion -> Bool
isEOL jvm | jvm <= java7 = True
isEOL jvm | jvm == java9 = True
isEOL jvm | jvm == java10 = True
isEOL jvm | jvm == java12 = True
isEOL jvm | jvm == java13 = True
isEOL jvm | jvm == java14 = True
isEOL jvm | jvm == java15 = True
isEOL jvm | jvm == java16 = True
isEOL jvm | jvm == java18 = True
isEOL jvm | jvm == java19 = True
isEOL _ = False

instance Pretty JVMVersion where
    pretty (JVMVersion v) = case v of
        50 -> "Java 6"
        51 -> "Java 7"
        52 -> "Java 8"
        53 -> "Java 9"
        54 -> "Java 10"
        55 -> "Java 11"
        56 -> "Java 12"
        57 -> "Java 13"
        58 -> "Java 14"
        59 -> "Java 15"
        60 -> "Java 16"
        61 -> "Java 17"
        62 -> "Java 18"
        63 -> "Java 19"
        64 -> "Java 20"
        65 -> "Java 21"
        _ -> "Unknown Version " <> pretty v
