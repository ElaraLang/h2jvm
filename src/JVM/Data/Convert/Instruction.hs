module JVM.Data.Convert.Instruction where

import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (..), ConstantPoolM, findIndexOf)
import JVM.Data.Abstract.Instruction as Abs (Instruction (..))
import JVM.Data.Raw.Instruction as Raw (Instruction (..))

convertInstruction :: Abs.Instruction -> ConstantPoolM Raw.Instruction
convertInstruction Abs.ALoad0 = pure Raw.ALoad0
convertInstruction (Abs.InvokeStatic c n m) = do
    idx <- findIndexOf (CPMethodRefEntry c n m)
    pure (Raw.InvokeStatic idx)
convertInstruction other = error ("Instruction not implemented: " <> show other)
