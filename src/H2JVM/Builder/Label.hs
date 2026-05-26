{- | Label type for instructions
This data type allows referring to specific instructions (for jumps) without having to manually calculate their offsets.
A label represents a __unique__ instruction in a Code attribute, and is marked by inserting a 'Label' instruction __directly above__ the instruction it refers to.
For example, the following (contrived) code:

> 0: aload_0
> 1: ifeq 4
> 4: return

could be written as follows, using 'H2JVM.Builder.Code.CodeBuilder':

@'H2JVM.Builder.Code.emit' 'H2JVM.Instruction.ALoad0'
label <- 'H2JVM.Builder.Code.newLabel'
'H2JVM.Builder.Code.emit' ('H2JVM.Instruction.IfEq' label)
'H2JVM.Builder.Code.emit' ('H2JVM.Instruction.Label' label) -- mark the label directly above the instruction it refers to
'H2JVM.Builder.Code.emit' 'H2JVM.Instruction.Return' @
-}
module H2JVM.Builder.Label (Label, unsafeMkLabel) where

import Data.Data

import H2JVM.Internal.Pretty

-- | A unique label for an instruction.
newtype Label = MkLabel Word
    deriving (Data, Eq, Ord, Show)

unsafeMkLabel :: Word -> Label
unsafeMkLabel = MkLabel

instance Pretty Label where
    pretty (MkLabel i) = "label_" <> pretty i
