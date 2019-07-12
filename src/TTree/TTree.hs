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
import Data.Semigroup (First(..))
import TTree.Branch
import Analysis.Const
import Analysis.Free


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


connectBranches :: TTree -> HM String VFP -> IO (Either String ())
connectBranches t hm =
  getCompose $ traverse_ (\(k, p) -> Compose $ connectBranch t k p) (HM.toList hm)


-- interpretBranches
--   :: Member arrs (Const (HM String (First (IO VFP))))
--   => U (L String BV2 ': L String BV ': L String BS ': arrs)
--   :-> U arrs
-- interpretBranches =
--   runU (inj <<< cnv <<< ptrBS)
--   <<< runU (inj <<< cnv <<< ptrBV)
--   <<< runU (inj <<< cnv <<< ptrBV2)
--   where
--     cnv :: Const (String, IO VFP) :-> Const (HM.MonoidalHashMap String (First (IO VFP)))
--     cnv = mapConst (\(s, p) -> HM.singleton s (First p))


-- compileBranches
--   :: Member arrs (Kleisli IO)
--   => (s -> VFP)
--   -> U (L s BV2 ': L s BV ': L s BS ': arrs)
--   :-> U arrs
-- compileBranches hook =
--   runU (inj <<< readBS <<< mapL hook)
--   <<< runU (inj <<< readBV <<< mapL hook)
--   <<< runU (inj <<< readBV2 <<< mapL hook)


ttree :: TFile -> String -> IO TTree
ttree f tn = do
  tp <- withCString tn (_ttree f)
  return $ TTree tp


isNullTree :: TTree -> Bool
isNullTree (TTree p) = p == nullPtr


loadEntry :: TTree -> Int -> IO Bool
loadEntry (TTree p) i = (>= 0) <$> _ttreeLoadTree p i



-- TODO
-- -- this really should be of type Int -> TTree -> (ExceptT TreeError m) a
-- -- but this is isomorphic, no?
-- type TreeRead m = ReaderT Int (StateT TTree m)
-- 
-- 
-- readEntry :: Monad m => TreeRead m Int
-- readEntry = ask
-- 
-- 
-- -- note: this assumes that once a branch has been requested
-- -- that we want to load it for *EVERY EVENT*
-- readBranchMaybe
--   :: (MonadIO m, Branchable a, Storable (HeapType a))
--   => String -> TreeRead m (Maybe a)
-- readBranchMaybe s = do
--   i <- ask
--   t <- get
--   case s `HM.lookup` ttreeBranches t of
--     -- we've already read this branch at least once: don't alloc a
--     -- new pointer
--     Just p  -> fmap Just . liftIO $ withForeignPtr (castForeignPtr p) fromB
-- 
--     -- this is the first time we've accessed this branch: alloc a
--     -- new pointer
--     Nothing -> do
--       p <- liftIO $ newForeignPtr_ =<< calloc
--       modify . const
--         $ t { ttreeBranches = HM.insert s (castForeignPtr p) (ttreeBranches t) }
--       n <-
--         liftIO
--           $ withCString s
--           $ \s' -> withForeignPtr (ttreePtr t)
--           $ \tp' -> withForeignPtr p
--           $ \p' -> _ttreeGetBranchEntry tp' s' i p'
-- 
--       if n > 0
--          then fmap Just . liftIO $ withForeignPtr p fromB
--          else return Nothing
-- 
-- 
-- data TTreeException = EndOfTTree | TBranchReadFailure String
--   deriving (Typeable, Show)
-- 
-- instance Exception TTreeException where
-- 
-- 
-- readBranch
--   :: ( MonadIO m, MonadThrow m
--      , Branchable a, Storable (HeapType a))
--   => String -> TreeRead m a
-- readBranch s = do
--   m <- readBranchMaybe s
--   case m of
--     Nothing -> do
--       i <- ask
--       throwM . TBranchReadFailure
--         $ "unable to read branch " ++ s ++ " for event number " ++ show i
--     Just x  -> return x
-- 
-- 
-- class FromTTree a where
--     fromTTree :: (MonadIO m, MonadThrow m) => TreeRead m a
-- 
-- 
-- 
-- 
-- pipeTTree
--   :: (MonadIO m, MonadState TTree m)
--   => ReaderT Int m b
--   -> Pipe Int b m ()
-- pipeTTree f = do
--   i <- await
--   mx <- lift $ readTTreeEntry f i
--   case mx of
--     Just x -> do
--       yield x
--       pipeTTree f
--     Nothing -> return ()
