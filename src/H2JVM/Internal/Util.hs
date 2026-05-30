module H2JVM.Internal.Util (bug) where

import GHC.Stack (HasCallStack)

-- | Raise an internal error. This simply throws an imprecise exception, but adds a more informative prefix.
bug :: HasCallStack => String -> a
bug msg = error $ "Internal bug in h2jvm: " <> msg
