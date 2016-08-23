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


{-
void ttreeSetBranchAddress(void*, const char*, void**);

unsigned int vectorSizeI(void* vp);
unsigned int vectorSizeC(void* vp);
unsigned int vectorSizeD(void* vp);
unsigned int vectorSizeF(void* vp);

int*    vectorDataI(void* vp);
char*   vectorDataC(void* vp);
double* vectorDataD(void* vp);
float*  vectorDataF(void* vp);
-}

foreign import ccall "ttreeC.h ttreeSetBranchAddress" ttreeSetBranchAddress :: Ptr a -> CString -> Ptr (Ptr b) -> IO ()


data TBranch a = TBScalar Text (Ptr a)
               | TBVector Text (Ptr a)

makePrisms ''TBranch


newtype TTree a = TTree { _unTTree :: [TBranch a] }

makeLenses ''TTree
