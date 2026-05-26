{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Convert (spec) where

import Data.Word (Word32)
import Effectful
import Hedgehog (MonadTest, forAll, property, (===))
import Test.Syd hiding (shouldContain)
import Test.Syd.Hedgehog ()

import Data.List.NonEmpty qualified as NE
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import H2JVM.Analyse.StackMap
import H2JVM.Builder.Code
import H2JVM.ClassFile.AccessFlags
import H2JVM.ConstantPool
import H2JVM.Descriptor (MethodDescriptor (MethodDescriptor), ReturnDescriptor (TypeReturn, VoidReturn))
import H2JVM.Instruction (Instruction' (..), LDCEntry (..))
import H2JVM.Internal.Convert (jloName)
import H2JVM.Internal.Convert.ConstantPool (ConstantPoolState (ConstantPoolState))
import H2JVM.Internal.Raw.ConstantPool (ConstantPoolInfo (..))
import H2JVM.Type (ClassInfoType (ClassInfoType), FieldType (..), PrimitiveType (Boolean, Char, Int))
import Util

import H2JVM.Internal.IndexedMap qualified as IM
import H2JVM.Internal.Raw.ClassFile qualified as Raw
import H2JVM.Internal.Raw.ConstantPool qualified as Raw
import H2JVM.Internal.Raw.Instruction qualified as Raw

spec :: Spec
spec = describe "test conversions" $ do
    frameAnalysis
    it "Converts a simple invokestatic instruction properly" $ property $ do
        ([inst], ConstantPoolState constants bms) <-
            runConv
                [ InvokeStatic
                    (ClassInfoType "java.lang.String")
                    "charAt"
                    (MethodDescriptor [PrimitiveFieldType Int] (TypeReturn (PrimitiveFieldType Char)))
                ]
        constants `shouldContain` Raw.UTF8Info "java/lang/String"
        constants `shouldContain` Raw.UTF8Info "charAt"
        constants `shouldContain` Raw.UTF8Info "(I)C"

        bms === []

        indexOfMethodRef <- findCPIndex (\case MethodRefInfo _ _ -> True; _ -> False) constants
        inst === Raw.InvokeStatic (fromIntegral indexOfMethodRef)

    it "Converts a simple ldc instruction properly" $ property $ do
        ([inst], ConstantPoolState constants bms) <- runConv [LDC $ LDCString "hello"]
        constants `shouldContain` Raw.UTF8Info "hello"
        bms === []
        indexOfInteger <- findCPIndex (\case StringInfo _ -> True; _ -> False) constants
        inst === Raw.LDC_W (fromIntegral indexOfInteger)

    it "Converts a simple indy instruction properly" $ property $ do
        ([inst], ConstantPoolState constants bms) <-
            runConv
                [ InvokeDynamic
                    ( BootstrapMethod
                        ( MHInvokeStatic
                            ( MethodRef
                                (ClassInfoType "java/lang/Object")
                                "equals"
                                (MethodDescriptor [ObjectFieldType "java/lang/Object"] (TypeReturn (PrimitiveFieldType Boolean)))
                            )
                        )
                        [BMStringArg "Hello"]
                    )
                    "foo"
                    (MethodDescriptor [PrimitiveFieldType Int] (TypeReturn (PrimitiveFieldType Char)))
                ]
        constants `shouldContain` Raw.UTF8Info "foo"
        constants `shouldContain` Raw.UTF8Info "(I)C"

        indexOfMethodHandle <- findCPIndex (\case MethodHandleInfo _ _ -> True; _ -> False) constants
        indexOfIndy <- findCPIndex (\case InvokeDynamicInfo _ _ -> True; _ -> False) constants
        strArgIndex <- findCPIndex (\case StringInfo _ -> True; _ -> False) constants

        bms === IM.singleton (Raw.BootstrapMethod (fromIntegral indexOfMethodHandle) [fromIntegral strArgIndex])
        inst === Raw.InvokeDynamic (fromIntegral indexOfIndy)

findCPIndex :: MonadTest m => (a -> Bool) -> IM.IndexedMap a -> m Word32
findCPIndex pred cp = fmap IM.indexValue $ shouldBeJust $ IM.lookupIndexWhere pred cp

frameAnalysis :: SpecWith ()
frameAnalysis = describe "Stack Map Frame Analysis" $ do
    it "Tracks stack height correctly for arbitrary linear sequences" $ property $ do
        ops <-
            forAll $
                Gen.filter (validStackFlow 0) $
                    Gen.list (Range.linear 0 50) (Gen.element @[] [1, -1])

        let (_, _, code) = runPureEff $ runCodeBuilder $ do
                mapM_ modifyStack ops
                emit Return
        let blocks = splitIntoBasicBlocks code
        let top = topFrame jloName [MStatic] (MethodDescriptor [] VoidReturn)
        let endFrame = analyseBlockDiff top (NE.head blocks)

        -- The final stack height should be the sum of all operations
        length endFrame.stack === sum ops

-- | Emits an arbitrary operation that changes the stack by the given amount
modifyStack :: CodeBuilder :> r => Int -> Eff r ()
modifyStack 1 = emit $ LDC (LDCInt 1)
modifyStack (-1) = emit $ IStore 0
modifyStack _ = pure ()

-- | Checks if a sequence of stack operations is valid, i.e. it never goes negative at any point
validStackFlow :: Int -> [Int] -> Bool
validStackFlow currentDepth [] = currentDepth >= 0
validStackFlow currentDepth (x : xs)
    | currentDepth < 0 = False
    | otherwise = validStackFlow (currentDepth + x) xs
