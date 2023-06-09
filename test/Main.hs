module Main where

import Convert (spec)
import Data.Binary.Put
import Data.Binary.Write (WriteBinary (..))
import Data.ByteString qualified as BS
import JVM.Data.Abstract.AccessFlags
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.Descriptor (MethodDescriptor (MethodDescriptor), ReturnDescriptor (..))
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Method (ClassFileMethod (ClassFileMethod), CodeAttributeData (CodeAttributeData), MethodAttribute (..))
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type
import JVM.Data.Convert
import JVM.Data.JVMVersion
import Test.Hspec (hspec)
import Text.Pretty.Simple (
    CheckColorTty (NoCheckColorTty),
    defaultOutputOptionsDarkBg,
    pPrintOpt,
    pShow,
 )

main :: IO ()
main = do
    hspec spec
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
                    [ Code $
                        CodeAttributeData
                            2
                            1
                            [ LDC
                                (LDCString "hello world!")
                            , InvokeStatic
                                ( ClassInfoType
                                    "Prelude"
                                )
                                "println"
                                ( MethodDescriptor
                                    [ ObjectFieldType "java.lang.String"
                                    ]
                                    ( TypeReturn
                                        ( ObjectFieldType
                                            "elara.IO"
                                        )
                                    )
                                )
                            , Return
                            ]
                            []
                            []
                    ]
                ]
                [SourceFile "out.java"]

    let classFile' = convert classFile
    pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg classFile'
    let bs = runPut (writeBinary classFile')
    BS.writeFile "out.class" (BS.toStrict bs)
