module JVM.Data.Convert.Instruction where

import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (..), FieldRef (..), MethodRef (..))
import JVM.Data.Abstract.Instruction as Abs (Instruction (..), LDCEntry (..))
import JVM.Data.Convert.ConstantPool
import JVM.Data.Raw.Instruction as Raw (Instruction (..))

convertInstruction :: Abs.Instruction -> ConstantPoolM Raw.Instruction
convertInstruction Abs.ALoad0 = pure Raw.ALoad0
convertInstruction Abs.ALoad1 = pure Raw.ALoad1
convertInstruction Abs.ALoad2 = pure Raw.ALoad2
convertInstruction Abs.ALoad3 = pure Raw.ALoad3
convertInstruction Abs.AThrow = pure Raw.AThrow
convertInstruction Abs.AConstNull = pure Raw.AConstNull
convertInstruction (Abs.InvokeStatic c n m) = do
    idx <- findIndexOf (CPMethodRefEntry (MethodRef c n m))
    pure (Raw.InvokeStatic idx)
convertInstruction (Abs.InvokeVirtual c n m) = do
    idx <- findIndexOf (CPMethodRefEntry (MethodRef c n m))
    pure (Raw.InvokeVirtual idx)
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
