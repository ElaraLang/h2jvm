{-# LANGUAGE OverloadedLists #-}

module Main where

import Builder qualified (spec)
import Convert qualified (spec)
import Data.Binary.Put
import Data.Binary.Write (WriteBinary (..))
import Data.ByteString qualified as BS
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.ClassFile.AccessFlags
import JVM.Data.Abstract.ClassFile.Method (ClassFileMethod (ClassFileMethod), CodeAttributeData (CodeAttributeData), MethodAttribute (..))
import JVM.Data.Abstract.Descriptor (MethodDescriptor (MethodDescriptor), ReturnDescriptor (..))
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type
import JVM.Data.Convert
import JVM.Data.JVMVersion
import Test.Hspec (Spec, hspec)
import Util (shouldBeRight)

spec :: Spec
spec = do
    Convert.spec
    Builder.spec

main :: IO ()
main = do
    hspec spec
   
