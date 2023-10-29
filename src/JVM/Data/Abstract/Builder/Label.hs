{- | Label type for instructions
This data type allows referring to specific instructions (for jumps) without having to manually calculate their offsets.
A label represents a __unique__ instruction in a Code attribute, and is marked by inserting a 'Label' instruction __directly above__ the instruction it refers to.
For example, the following (contrived) code:

>        0: aload_0
>        1: ifeq          4
>        4: return

could be written as follows, using 'JVM.Data.Abstract.Builder.Code.CodeBuilder':

> emit ALoad0
> label <- newLabel
> emit (IfEq label)
> emit (Label label)
> emit Return
-}
module JVM.Data.Abstract.Builder.Label where

newtype Label = MkLabel Int
    deriving (Show, Eq, Ord)
