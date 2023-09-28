module Util where

import Test.Hspec (HasCallStack, expectationFailure)

shouldBeJust :: HasCallStack => Maybe a -> IO a
shouldBeJust (Just a) = pure a
shouldBeJust Nothing = undefined <$ expectationFailure "Expected Just, got Nothing" -- This is safe because we know that the expectationFailure function will never return

shouldContain :: (HasCallStack, Eq a, Show a, Foldable t, Show (t a)) => t a -> a -> IO ()
shouldContain t a = if a `elem` t then pure () else expectationFailure ("Expected " ++ show a ++ " to be in " ++ show t)
