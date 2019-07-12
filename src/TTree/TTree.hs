{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts  #-}

module TTree.TTree where

import Prelude hiding (id, (.))
import Foreign
import Foreign.C.Types
import Foreign.C.String
import TTree.TFile
import qualified Data.HashMap.Monoidal as HM
import TTree.Internal.Common
import Data.Functor.Compose
import Data.Foldable (traverse_)


type HM = HM.MonoidalHashMap


foreign import ccall "ttreeC.h ttree" _ttree
  :: TFile -> CString -> IO VP
foreign import ccall "ttreeC.h ttreeLoadTree" _ttreeLoadTree
  :: VP -> Int -> IO CLong
foreign import ccall "ttreeC.h ttreeGetBranchEntry" _ttreeGetBranchEntry
  :: VP -> CString -> Int -> Ptr a -> IO Int
foreign import ccall "ttreeC.h &ttreeFree" _ttreeFree
  :: FunPtr (VP -> IO ())


newtype TTree = TTree { ttreeAddr :: VP }


connectBranch :: TTree -> String -> VFP -> IO (Either String ())
connectBranch (TTree ttreeptr) k p =
  withCString k $ \k' ->
    withForeignPtr p $ \p' -> do
      n <- _ttreeGetBranchEntry ttreeptr k' 0 p'
      case n of
        0 -> return <<< Left $ "missing branch \"" ++ k ++ "\""
        _ -> return $ Right ()
{-# INLINE connectBranch  #-}


connectBranches :: TTree -> HM String VFP -> IO (Either String ())
connectBranches t hm =
  getCompose $ traverse_ (\(k, p) -> Compose $ connectBranch t k p) (HM.toList hm)
{-# INLINE connectBranches  #-}


ttree :: TFile -> String -> IO TTree
ttree f tn = do
  tp <- withCString tn (_ttree f)
  return $ TTree tp


isNullTree :: TTree -> Bool
isNullTree (TTree p) = p == nullPtr


loadEntry :: TTree -> Int -> IO Bool
loadEntry (TTree p) i = (>= 0) <$> _ttreeLoadTree p i
