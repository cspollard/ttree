{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.TTree where


import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String

import Control.Lens
import Conduit
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Data.Text (Text)


data TChain
data TBranch
data TTree

foreign import ccall "ttreeC.h tchain" tchain :: CString -> IO (Ptr TChain)
foreign import ccall "ttreeC.h tchainAdd" tchainAdd :: Ptr TChain -> CString -> IO ()
foreign import ccall "ttreeC.h tchainGetEntry" tchainGetEntry :: Ptr TChain -> Int -> IO ()


{-
data TBranch a = TBScalar Text (Ptr a)
               | TBVector Text (Ptr a)

makePrisms ''TBranch


newtype TTree a = TTree { _unTTree :: [TBranch a] }

makeLenses ''TTree
-}
