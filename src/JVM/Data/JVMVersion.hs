-- | Information about JVM Versions
module JVM.Data.JVMVersion where

import Data.Binary (Word16)

-- | A JVM Version Number
newtype JVMVersion = JVMVersion Word16 deriving (Eq, Ord, Show)

newtype MajorVersion = MajorVersion Word16 deriving (Eq, Ord, Num, Show)

newtype MinorVersion = MinorVersion Word16 deriving (Eq, Ord, Num, Show)

unwrapMajor :: MajorVersion -> Word16
unwrapMajor (MajorVersion v) = v

unwrapMinor :: MinorVersion -> Word16
unwrapMinor (MinorVersion v) = v

getMajor :: JVMVersion -> MajorVersion
getMajor (JVMVersion v) = MajorVersion v

getMinor :: JVMVersion -> MinorVersion
getMinor _ = MinorVersion 0 -- currently all JVM versions are 0

java6 :: JVMVersion
java6 = JVMVersion 50

java7 :: JVMVersion
java7 = JVMVersion 51

java8 :: JVMVersion
java8 = JVMVersion 52

java9 :: JVMVersion
java9 = JVMVersion 53

java10 :: JVMVersion
java10 = JVMVersion 54

java11 :: JVMVersion
java11 = JVMVersion 55

java12 :: JVMVersion
java12 = JVMVersion 56

java13 :: JVMVersion
java13 = JVMVersion 57

java14 :: JVMVersion
java14 = JVMVersion 58

java15 :: JVMVersion
java15 = JVMVersion 59

java16 :: JVMVersion
java16 = JVMVersion 60

java17 :: JVMVersion
java17 = JVMVersion 61

java18 :: JVMVersion
java18 = JVMVersion 62

java19 :: JVMVersion
java19 = JVMVersion 63

java20 :: JVMVersion
java20 = JVMVersion 64

java21 :: JVMVersion
java21 = JVMVersion 65

isLTS :: JVMVersion -> Bool
isLTS jvm | jvm == java8 = True
isLTS jvm | jvm == java11 = True
isLTS jvm | jvm == java17 = True
isLTS jvm | jvm == java21 = True
isLTS _ = False

{- | Returns true if the JVM version is EOL at the time of writing (April 2023)
This is for OpenJDK, other vendors may have different EOL dates
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
