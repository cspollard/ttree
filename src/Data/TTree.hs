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
  , TR, FromTTree(..)
  , produceTTree, runTTree
  , MonadIO(..)
  ) where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans        (lift)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.TBranch
import           Data.TFile
import           Foreign                    hiding (void)
import           Foreign.C.String
import           Pipes


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
type TR m = ReaderT Int (StateT TTree m)

-- note: this assumes that once a branch has been requested
-- that we want to load it for *EVERY EVENT*
readBranchMaybe
  :: (MonadIO m, Branchable a, Storable (HeapType a), Freeable (HeapType a))
  => String -> TR m (Maybe a)
readBranchMaybe s = do
  t <- get

  maybe (allocPtr >>= readPtr) readPtr (s `M.lookup` ttreeBranches t)


  where
    readPtr p = do
      t <- get
      i <- ask
      n <-
        liftIO
          $ withCString s
          $ \s' -> withForeignPtr (ttreePtr t)
          $ \tp' -> withForeignPtr p
          $ \p' -> _ttreeGetBranchEntry tp' s' i p'

      if n > 0
         then fmap Just . liftIO $ withForeignPtr (castForeignPtr p) fromB
         else return Nothing

    allocPtr = do
      t <- get
      p <- liftIO $ newForeignPtr_ =<< calloc
      modify . const
        $ t { ttreeBranches = M.insert s (castForeignPtr p) (ttreeBranches t) }
      return p


-- TODO
-- we should use MonadThrow here
-- but the laziness is killing me.
-- fail if the branch doesn't exist or is unreadable.
readBranch
  :: (MonadIO m, Branchable a, Storable (HeapType a), Freeable (HeapType a))
  => String -> TR m a
readBranch s = do
  m <- readBranchMaybe s
  case m of
    Nothing -> error $ "unable to read branch " ++ s
    Just x  -> return x


class FromTTree a where
    fromTTree :: MonadIO m => TR m a

runTR :: Monad m => TR m a -> Int -> Producer a (StateT TTree m) ()
runTR f i = lift (runReaderT f i) >>= yield

produceTTree :: MonadIO m => TR m a -> Producer a (StateT TTree m) ()
produceTTree f = for ints $ runTR f
  where ints = each [0..]


runTTree :: Monad m => TTree -> Effect (StateT TTree m) a -> m a
runTTree t = flip evalStateT t . runEffect

-- foldMTTree
--   :: Monad m
--   => (x -> a -> TR m x)
--   -> TR m x
--   -> (x -> TR m b)
--   -> TTree
--   -> Producer a (TR m) ()
--   -> m b
-- foldMTTree comb start done t prod =
--   runTR t $ P.foldM comb start done prod
--
-- foldTTree
--   :: Monad m
--   => (x -> a -> x)
--   -> x
--   -> (x -> b)
--   -> TTree
--   -> Producer a (TR m) ()
--   -> m b
-- foldTTree comb start done t prod =
--   runTR t $ P.fold comb start done prod
