{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.TTree
  ( ttree, TTree, isNullTree
  , module Data.TBranch
  , readBranch, readBranchMaybe
  , TreeRead, FromTTree(..)
  , produceTTree, runTTree
  , MonadIO(..)
  ) where

import           Control.Monad.Fail
import           Control.Monad.Reader       hiding (fail)
import           Control.Monad.State.Strict hiding (fail)
import           Control.Monad.Trans        (lift)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.TBranch
import           Data.TFile
import           Foreign                    hiding (void)
import           Foreign.C.String
import           Pipes
import           Prelude                    hiding (fail)


-- void pointer
type VPtr = Ptr ()
type FVPtr = ForeignPtr ()

foreign import ccall "ttreeC.h ttree" _ttree
    :: TFile -> CString -> IO VPtr
foreign import ccall "ttreeC.h ttreeLoadTree" _ttreeLoadTree
    :: VPtr -> Int -> IO CLong
foreign import ccall "ttreeC.h ttreeGetBranchEntry" _ttreeGetBranchEntry
    :: VPtr -> CString -> Int -> Ptr a -> IO Int
foreign import ccall "ttreeC.h &ttreeFree" _ttreeFree
    :: FunPtr (Ptr () -> IO ())


data TTree =
  TTree
    { ttreePtr      :: FVPtr
    , ttreeBranches :: Map String FVPtr
    }

ttree :: TFile -> String -> IO TTree
ttree f tn = do
    tp <- newForeignPtr_ =<< withCString tn (_ttree f)
    return $ TTree tp M.empty

isNullTree :: TTree -> IO Bool
isNullTree (TTree p _) = withForeignPtr p (return . (== nullPtr))

-- TODO
-- this really should be of type Int -> TTree -> (ExceptT TreeError m) a
-- but this is isomorphic, no?
type TreeRead m = ReaderT Int (StateT TTree m)

-- note: this assumes that once a branch has been requested
-- that we want to load it for *EVERY EVENT*
readBranchMaybe
  :: (MonadIO m, Branchable a, Storable (HeapType a), Freeable (HeapType a))
  => String -> TreeRead m (Maybe a)
readBranchMaybe s = do
  i <- ask
  t <- get
  case s `M.lookup` ttreeBranches t of
    -- we've already read this branch at least once: don't alloc a
    -- new pointer
    Just p  -> fmap Just . liftIO $ withForeignPtr (castForeignPtr p) fromB

    -- this is the first time we've accessed this branch: alloc a
    -- new pointer
    Nothing -> do
      p <- liftIO $ newForeignPtr_ =<< calloc
      modify . const
        $ t { ttreeBranches = M.insert s (castForeignPtr p) (ttreeBranches t) }
      n <-
        liftIO
          $ withCString s
          $ \s' -> withForeignPtr (ttreePtr t)
          $ \tp' -> withForeignPtr p
          $ \p' -> _ttreeGetBranchEntry tp' s' i p'

      if n > 0
         then fmap Just . liftIO $ withForeignPtr p fromB
         else return Nothing

-- TODO
-- we should use MonadThrow here
-- but the laziness is killing me.
-- fail if the branch doesn't exist or is unreadable.
readBranch
  :: (MonadIO m, Branchable a, Storable (HeapType a), Freeable (HeapType a), MonadFail m)
  => String -> TreeRead m a
readBranch s = do
  m <- readBranchMaybe s
  case m of
    Nothing -> fail $ "unable to read branch " ++ s
    Just x  -> return x


class FromTTree a where
    fromTTree :: (MonadIO m, MonadFail m) => TreeRead m a

runTreeRead :: (MonadIO m, MonadFail m) => TreeRead m a -> Int -> Producer a (StateT TTree m) ()
runTreeRead tr i = do
  x <- lift . flip runReaderT i $ do
    t <- get
    n <- liftIO $ withForeignPtr (ttreePtr t) $ flip _ttreeLoadTree i
    if n >= 0 then tr else fail "no bytes read in."

  yield x


produceTTree :: (MonadFail m, MonadIO m) => TreeRead m a -> Producer a (StateT TTree m) ()
produceTTree f = for ints $ runTreeRead f
  where ints = each [0..]

runTTree :: Monad m => s -> Effect (StateT s m) a -> m a
runTTree t = flip evalStateT t . runEffect

-- foldMTTree
--   :: Monad m
--   => (x -> a -> TreeRead m x)
--   -> TreeRead m x
--   -> (x -> TreeRead m b)
--   -> TTree
--   -> Producer a (TreeRead m) ()
--   -> m b
-- foldMTTree comb start done t prod =
--   runTreeRead t $ P.foldM comb start done prod
--
-- foldTTree
--   :: Monad m
--   => (x -> a -> x)
--   -> x
--   -> (x -> b)
--   -> TTree
--   -> Producer a (TreeRead m) ()
--   -> m b
-- foldTTree comb start done t prod =
--   runTreeRead t $ P.fold comb start done prod
