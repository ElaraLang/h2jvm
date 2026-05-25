module StackMap where

import Effectful
import Hedgehog (property, (===))
import Test.Syd
import Test.Syd.Hedgehog ()

import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.ClassFile.AccessFlags (MethodAccessFlag (..))
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type
import JVM.Data.Analyse.StackMap
import JVM.Data.Convert

import JVM.Data.Abstract.Type qualified as JVM

spec :: Spec
spec = describe "StackMap Analysis Tests" $ do
    describe "Max Stack and Locals Calculation" $ do
        it "Calculates max stack and locals accurately for 1-slot types" $ do
            let (_, _, code) = runPureEff $ runCodeBuilder $ do
                    emit IConst1
                    emit IConst1
                    emit IAdd
                    emit $ IStore 1
                    emit Return

            property $ do
                let md = MethodDescriptor [] VoidReturn
                let (_, maxStack, maxLocals) = calculateStackMapFrames jloName [MStatic] md code

                maxStack === 2
                maxLocals === 2
        it "Accounts for Double & Long requiring 2 slots" $ do
            let (_, _, code) = runPureEff $ runCodeBuilder $ do
                    emit $ GetStatic (ClassInfoType "Math") "PI" (PrimitiveFieldType JVM.Double)
                    emit IConst1
                    emit Return

            property $ do
                let md = MethodDescriptor [PrimitiveFieldType JVM.Double, PrimitiveFieldType JVM.Int] VoidReturn
                let (_, maxStack, maxLocals) = calculateStackMapFrames jloName [MStatic] md code

                maxStack === 3
                maxLocals === 3 -- 2 slots for double param, 1 slot for int param
        it "Detects peak stack size hidden in the middle of a basic block" $ do
            let (_, _, code) = runPureEff $ runCodeBuilder $ do
                    emit IConst1
                    emit IConst1
                    emit IConst1
                    emit IConst1
                    -- peak = 4
                    emit IAdd
                    emit IAdd
                    emit IAdd
                    -- stack = 1
                    emit Return

            property $ do
                let md = MethodDescriptor [] VoidReturn
                let (_, maxStack, maxLocals) = calculateStackMapFrames jloName [MStatic] md code

                -- peak stack is 4, even though we end with only 1 on the stack
                maxStack === 4
                maxLocals === 0

        it "Includes 'this' in max_locals for instance methods" $ do
            let (_, _, code) = runPureEff $ runCodeBuilder $ do
                    emit Return

            property $ do
                let md = MethodDescriptor [PrimitiveFieldType Int] VoidReturn
                let (_, maxStack, maxLocals) = calculateStackMapFrames jloName [] md code

                maxStack === 0
                maxLocals === 2

        describe "Wide Type Memory Layout Checks" $ do
            it "topFrame explicitly pads Wide method arguments with Uninitialised slots" $ do
                property $ do
                    let md = MethodDescriptor [PrimitiveFieldType Double, PrimitiveFieldType Int] VoidReturn

                    let startFrame = topFrame jloName [MStatic] md

                    startFrame.locals
                        === [ LocalVariable (PrimitiveFieldType Double)
                            , Uninitialised
                            , LocalVariable (PrimitiveFieldType Int)
                            ]

            it "stores correctly invalidates the subsequent index when saving a wide type" $ do
                let block =
                        BasicBlock
                            0
                            [ GetStatic (ClassInfoType "Math") "PI" (PrimitiveFieldType Double)
                            , IStore 1 -- store the double into index 1, which should invalidate index 2
                            ]
                            Nothing
                            Nothing

                property $ do
                    let initialLocals = replicate 4 (LocalVariable (PrimitiveFieldType Int))
                    let top = Frame{locals = initialLocals, stack = []}

                    let resultingFrame = analyseBlockDiff top block

                    resultingFrame.locals
                        === [ LocalVariable (PrimitiveFieldType Int) -- unchanged
                            , LocalVariable (PrimitiveFieldType Double) -- the double we stored
                            , Uninitialised -- the slot after the double must be invalidated
                            , LocalVariable (PrimitiveFieldType Int) -- unchanged
                            ]
