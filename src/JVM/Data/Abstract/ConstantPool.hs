{-# LANGUAGE PartialTypeSignatures #-}

{- | Provides a monadic interface to the constant pool.
This aims to eliminate the need to manually specify the index of the constant
-}
module JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (..), findIndexOf, runConstantPoolM, ConstantPoolM) where

import Control.Monad.State
import Data.IndexedMap (IndexedMap)
import Data.IndexedMap qualified as IM
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import Data.Word (Word16)
import JVM.Data.Convert.Numbers (toJVMFloat, toJVMLong)
import JVM.Data.Raw.ConstantPool (ConstantPoolInfo (..))

{- | High level representation of a constant pool entry
This tries to hide indexes as much as possible -- some entries require expansion to multiple entries
-}
data ConstantPoolEntry
    = -- | A class reference
      CPClassEntry
        Text
        -- ^ The name of the class
    | CPFieldRefEntry Text ()
    | CPMethodRefEntry Text ()
    | CPInterfaceMethodRefEntry Text ()
    | CPStringEntry Text
    | CPIntegerEntry Int
    | CPFloatEntry Float
    | CPLongEntry Int64
    | CPDoubleEntry Double
    | CPNameAndTypeEntry Text Text
    | CPUTF8Entry Text
    | CPMethodHandleEntry Int -- TODO figure this one out
    | CPMethodTypeEntry Text -- TODO and this one
    | CPInvokeDynamicEntry Int -- TODO and this one
    deriving (Show, Eq, Ord)

transformEntry :: ConstantPoolEntry -> State (IndexedMap ConstantPoolInfo) Int
transformEntry (CPUTF8Entry text) = IM.lookupOrInsertM (UTF8Info $ encodeUtf8 text)
transformEntry (CPIntegerEntry i) = IM.lookupOrInsertM (IntegerInfo $ fromIntegral i)
transformEntry (CPFloatEntry f) = IM.lookupOrInsertM (FloatInfo (toJVMFloat f))
transformEntry (CPStringEntry msg) = do
    i <- transformEntry (CPUTF8Entry msg)
    IM.lookupOrInsertM (StringInfo $ fromIntegral i)
transformEntry (CPLongEntry i) = do
    let (high, low) = toJVMLong i
    IM.lookupOrInsertM (LongInfo high low)
transformEntry (CPDoubleEntry d) = do
    let (high, low) = toJVMLong (round d)
    IM.lookupOrInsertM (DoubleInfo high low)
transformEntry (CPClassEntry name) = do
    nameIndex <- transformEntry (CPUTF8Entry name)
    IM.lookupOrInsertM (ClassInfo $ fromIntegral nameIndex)
transformEntry _ = error "transformEntry"

type ConstantPoolM a = State (IndexedMap ConstantPoolInfo) a

findIndexOf :: ConstantPoolEntry -> ConstantPoolM Word16
findIndexOf = fmap fromIntegral . transformEntry

runConstantPoolM :: ConstantPoolM a -> (a, Vector ConstantPoolInfo)
runConstantPoolM m = let (a, cp) = runState m IM.empty in (a, IM.toVector cp)
