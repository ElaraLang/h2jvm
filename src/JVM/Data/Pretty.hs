
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module JVM.Data.Pretty (showPretty, tracePrettyId, Pretty(..), (<+>), hsep) where

import Data.String
import Prettyprinter 
import Prettyprinter.Render.Text (renderStrict)
import Data.Text (unpack)
import Debug.Trace (trace)

showPretty :: IsString s => Pretty a => a -> s
showPretty = fromString . unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty

instance Pretty (Doc a) where
    pretty = unAnnotate 
tracePrettyId :: (Pretty a) => a -> a
tracePrettyId a = trace (showPretty a) a