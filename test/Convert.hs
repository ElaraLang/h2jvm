{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Convert where

import Data.IndexedMap qualified as IM
import Effectful
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.ConstantPool
import JVM.Data.Abstract.Descriptor (MethodDescriptor (MethodDescriptor), ReturnDescriptor (TypeReturn, VoidReturn))
import JVM.Data.Abstract.Instruction (Instruction' (..), LDCEntry (..))
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type (ClassInfoType (ClassInfoType), FieldType (..), PrimitiveType (Boolean, Char, Int))
import JVM.Data.Analyse.StackMap
import JVM.Data.Convert.ConstantPool (ConstantPoolState (ConstantPoolState))
import JVM.Data.Raw.ClassFile qualified as Raw
import JVM.Data.Raw.ConstantPool (ConstantPoolInfo (..))
import JVM.Data.Raw.ConstantPool qualified as Raw
import JVM.Data.Raw.Instruction qualified as Raw
import Test.Hspec hiding (shouldContain)
import Test.Hspec.Hedgehog
import Util
import JVM.Data.Convert (jloName)
import JVM.Data.Abstract.ClassFile.AccessFlags


spec :: Spec
spec = describe "test conversions" $ do
    frameAnalysis
    it "Converts a simple invokestatic instruction properly" $ hedgehog $ do
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

    it "Converts a simple ldc instruction properly" $ hedgehog $ do
        ([inst], ConstantPoolState constants bms) <- runConv [LDC $ LDCString "hello"]
        constants `shouldContain` Raw.UTF8Info "hello"
        bms === []
        indexOfInteger <- findCPIndex (\case StringInfo _ -> True; _ -> False) constants
        inst === Raw.LDC_W (fromIntegral indexOfInteger)

    it "Converts a simple indy instruction properly" $ hedgehog $ do
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

findCPIndex :: (MonadTest m) => (a -> Bool) -> IM.IndexedMap a -> m Int
findCPIndex pred cp = shouldBeJust $ IM.lookupIndexWhere pred cp

frameAnalysis :: SpecWith ()
frameAnalysis = describe "Stack Map Frame Analysis" $ do
    it "Tracks stack height correctly for arbitrary linear sequences" $ hedgehog $ do
        ops <-
            forAll $
                Gen.filter (validStackFlow 0) $
                    Gen.list (Range.linear 0 50) (Gen.element @[] [1, -1])

        let (_, _, code) = runPureEff $ runCodeBuilder $ do
                mapM_ emitOp ops
                emit Return
        let blocks = splitIntoBasicBlocks code
        let top = topFrame jloName [MStatic] (MethodDescriptor [] VoidReturn)
        let endFrame = analyseBlockDiff top (head blocks)

        -- The final stack height should be the sum of all operations
        length endFrame.stack === sum ops

-- | Emits an operation that changes the stack by the given amount
emitOp :: (CodeBuilder :> r) => Int -> Eff r ()
emitOp 1 = emit $ LDC (LDCInt 1)
emitOp (-1) = emit $ IStore 0
emitOp _ = pure ()

-- | Checks if a sequence of stack operations is valid (never goes negative)
validStackFlow :: Int -> [Int] -> Bool
validStackFlow currentDepth [] = currentDepth >= 0
validStackFlow currentDepth (x : xs)
    | currentDepth < 0 = False
    | otherwise = validStackFlow (currentDepth + x) xs