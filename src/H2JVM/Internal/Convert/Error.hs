module H2JVM.Internal.Convert.Error (CodeConverterError (..)) where

import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word16, Word32)

import H2JVM.Builder.Label

import H2JVM.Instruction qualified as Abs

-- | Errors that may be thrown during code conversion.
data CodeConverterError
    = -- | A 'Label' was used more than once.
      DuplicateLabel
        -- | The 'Label' in question.
        Label
        Word16 -- The offset of the second encountered instance of the 'Label'
    | -- | A 'Label' was referenced but never had its location defined.
      UnmarkedLabel
        Label
    | -- | the attribute in question is not yet supported. This error will be removed at some point -- unsupported attributes will simply not be defined if they are unsupported :)
      UnsupportedAttribute String
    | -- | the constant pool has too many attributes, more than the JVM limit of 65,536
      ConstantPoolOverflow
    | -- | none of the given instructions could be converted into any concrete result, e.g. they are all 'Label's
      NoValidInstructions
        -- | the instructions provided
        (NonEmpty Abs.Instruction)
    deriving (Show)
