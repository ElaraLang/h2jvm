module Main where

import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.Name
import JVM.Data.Convert
import JVM.Data.JVMVersion

import Data.ByteString qualified as BS

import Data.Binary.Put
import Data.Binary.Write (WriteBinary (..))
import JVM.Data.Abstract.AccessFlags
import JVM.Data.Abstract.Method (ClassFileMethod (ClassFileMethod), MethodDescriptor (..), ReturnDescriptor (VoidReturn))
import JVM.Data.Abstract.Type

main :: IO ()
main = do
    let classFile =
            ClassFile
                "out"
                java17
                [Public, Final]
                (Just "java.lang.Object")
                ["java.util.List"]
                []
                [ ClassFileMethod
                    [MPublic, MStatic]
                    "main"
                    (MethodDescriptor [ObjectFieldType (parseQualifiedClassName "java.lang.String")] VoidReturn)
                    []
                ]
                [SourceFile "out.java"]

    let classFile' = convert classFile
    let bs = runPut (writeBinary classFile')
    BS.writeFile "out.class" (BS.toStrict bs)
