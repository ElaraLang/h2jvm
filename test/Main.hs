module Main where

import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.JVMVersion
import JVM.Data.Abstract.Name
import JVM.Data.Convert

import Data.ByteString qualified as BS

import Data.Binary.Put
import Data.Binary.Write (WriteBinary (..))
import JVM.Data.Abstract.AccessFlags

main :: IO ()
main = do
    let classFile =
            ClassFile
                (parseQualifiedClassName "java.lang.Test")
                java17
                [Public, Final]
                (Just (parseQualifiedClassName "java.lang.Object"))
                []
    let classFile' = convert classFile
    let bs = runPut (writeBinary classFile')
    BS.writeFile "out.class" (BS.toStrict bs)
