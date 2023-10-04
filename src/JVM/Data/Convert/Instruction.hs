module JVM.Data.Convert.Instruction where

import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (..), FieldRef (..), MethodRef (..))
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction as Abs (Instruction (..), LDCEntry (..))
import JVM.Data.Abstract.Type
import JVM.Data.Convert.ConstantPool
import JVM.Data.Raw.Instruction as Raw (Instruction (..))

countArguments :: MethodDescriptor -> Int
countArguments (MethodDescriptor args _) = 1 + sum (map countArgument args)
  where
    countArgument :: FieldType -> Int
    countArgument (PrimitiveFieldType Double) = 2
    countArgument (PrimitiveFieldType Long) = 2
    countArgument _ = 1

convertInstruction :: Abs.Instruction -> ConstantPoolM Raw.Instruction
convertInstruction Abs.ALoad0 = pure Raw.ALoad0
convertInstruction Abs.ALoad1 = pure Raw.ALoad1
convertInstruction Abs.ALoad2 = pure Raw.ALoad2
convertInstruction Abs.ALoad3 = pure Raw.ALoad3
convertInstruction (Abs.ALoad 0) = pure Raw.ALoad0
convertInstruction (Abs.ALoad 1) = pure Raw.ALoad1
convertInstruction (Abs.ALoad 2) = pure Raw.ALoad2
convertInstruction (Abs.ALoad 3) = pure Raw.ALoad3
convertInstruction (Abs.ALoad idx) = pure (Raw.ALoad idx)
convertInstruction Abs.AStore0 = pure Raw.AStore0
convertInstruction Abs.AStore1 = pure Raw.AStore1
convertInstruction Abs.AStore2 = pure Raw.AStore2
convertInstruction Abs.AStore3 = pure Raw.AStore3
convertInstruction (Abs.AStore 0) = pure Raw.AStore0
convertInstruction (Abs.AStore 1) = pure Raw.AStore1
convertInstruction (Abs.AStore 2) = pure Raw.AStore2
convertInstruction (Abs.AStore 3) = pure Raw.AStore3
convertInstruction (Abs.AStore idx) = pure (Raw.AStore idx)
convertInstruction Abs.AThrow = pure Raw.AThrow
convertInstruction Abs.AConstNull = pure Raw.AConstNull
convertInstruction (Abs.InvokeStatic c n m) = do
    idx <- findIndexOf (CPMethodRefEntry (MethodRef c n m))
    pure (Raw.InvokeStatic idx)
convertInstruction (Abs.InvokeVirtual c n m) = do
    idx <- findIndexOf (CPMethodRefEntry (MethodRef c n m))
    pure (Raw.InvokeVirtual idx)
convertInstruction (Abs.InvokeInterface c n m) = do
    idx <- findIndexOf (CPInterfaceMethodRefEntry (MethodRef c n m))
    let count = countArguments m
    pure (Raw.InvokeInterface idx (fromIntegral count))
convertInstruction (Abs.LDC ldc) = do
    idx <-
        findIndexOf
            ( case ldc of
                LDCInt i -> CPIntegerEntry i
                LDCFloat f -> CPFloatEntry f
                LDCString s -> CPStringEntry s
                LDCClass c -> CPClassEntry c
            )

    pure (Raw.LDC (fromIntegral idx)) -- for some reason, the index is a u8, not a u16
    -- TODO: this should probably do a bounds check on the index
convertInstruction (Abs.PutStatic c n t) = do
    idx <- findIndexOf (CPFieldRefEntry (FieldRef c n t))
    pure (Raw.PutStatic idx)
convertInstruction (Abs.GetStatic c n t) = do
    idx <- findIndexOf (CPFieldRefEntry (FieldRef c n t))
    pure (Raw.GetStatic idx)
convertInstruction Abs.AReturn = pure Raw.AReturn
convertInstruction Abs.Return = pure Raw.Return
convertInstruction (Abs.CheckCast t) = do
    idx <- findIndexOf (CPClassEntry t)
    pure (Raw.CheckCast idx)
convertInstruction (Abs.InvokeDynamic bm n m) = do
    idx <- findIndexOf (CPInvokeDynamicEntry bm n m)
    pure (Raw.InvokeDynamic idx)
convertInstruction (Abs.IfEq offset) = pure (Raw.IfEq (fromIntegral offset))
convertInstruction (Abs.IfNe offset) = pure (Raw.IfNe (fromIntegral offset))
convertInstruction (Abs.IfLt offset) = pure (Raw.IfLt (fromIntegral offset))
convertInstruction (Abs.IfGe offset) = pure (Raw.IfGe (fromIntegral offset))
convertInstruction (Abs.IfGt offset) = pure (Raw.IfGt (fromIntegral offset))
convertInstruction (Abs.IfLe offset) = pure (Raw.IfLe (fromIntegral offset))

