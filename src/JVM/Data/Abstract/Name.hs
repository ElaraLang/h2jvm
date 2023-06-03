module JVM.Data.Abstract.Name where

import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T

{- | A JVM package name
This is defined as a potentially empty list of identifiers, which would be separated by dots in the source code
-}
newtype PackageName = PackageName [Text] deriving (Show, Eq, Ord)

{- | Parse a 'PackageName' from a 'Text'

>>> parsePackageName "java.lang"
PackageName ["java","lang"]

>>> parsePackageName ""
PackageName []
-}
parsePackageName :: Text -> PackageName
parsePackageName t = case T.splitOn "." t of
    [""] -> PackageName []
    xs -> PackageName xs

-- | A JVM class name
newtype ClassName = ClassName Text deriving (Show, Eq, Ord)

-- | Parse a 'ClassName' from a 'Text'
parseClassName :: Text -> ClassName
parseClassName = ClassName

data QualifiedClassName = QualifiedClassName PackageName ClassName deriving (Show, Eq, Ord)

instance IsString QualifiedClassName where
    fromString = parseQualifiedClassName . T.pack

{- | Parse a 'QualifiedClassName' from a 'Text'

>>> parseQualifiedClassName "java.lang.Object"
QualifiedClassName (PackageName ["java","lang"]) (ClassName "Object")

>>> parseQualifiedClassName "Object"
QualifiedClassName (PackageName []) (ClassName "Object")

This function is lenient and will accept invalid class names:
>>> parseQualifiedClassName "123invalid"
QualifiedClassName (PackageName []) (ClassName "123invalid")
-}
parseQualifiedClassName :: Text -> QualifiedClassName
parseQualifiedClassName t = case T.splitOn "." t of
    [c] -> QualifiedClassName (PackageName []) (ClassName c)
    xs -> QualifiedClassName (PackageName $ init xs) (ClassName $ last xs)

{- | Convert a 'QualifiedClassName' to a 'Text' that can be used as an internal JVM name

>>> toInternalName (QualifiedClassName (PackageName ["java","lang"]) (ClassName "Object"))
"java/lang/Object"
-}
toInternalName :: QualifiedClassName -> Text
toInternalName (QualifiedClassName (PackageName []) (ClassName c)) = c
toInternalName (QualifiedClassName (PackageName p) (ClassName c)) = T.intercalate "/" (p <> [c])

{- | Convert a 'QualifiedClassName' to a 'FilePath' that one would expect the class file to be in
>>> suitableFilePath "java.lang.Object"
"java/lang/Object.class"

>>> suitableFilePath "Object"
"Object.class"
-}
suitableFilePath :: QualifiedClassName -> FilePath
suitableFilePath (QualifiedClassName (PackageName p) (ClassName c)) = T.unpack $ T.intercalate "/" (p <> [c <> ".class"])
