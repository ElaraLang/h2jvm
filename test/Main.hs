module Main where

import JVM.Data.Abstract.ClassFile
import JVM.Data.JVMVersion
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
                "out"
                java17
                [Public, Final]
                (Just "java.lang.Object")
                ["java.util.List"]
    let classFile' = convert classFile
    let bs = runPut (writeBinary classFile')
    BS.writeFile "out.class" (BS.toStrict bs)
