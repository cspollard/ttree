{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.TTree
  ( module X
  , ttree, TTree, isNullTree
  , module Data.TBranch
  , TreeRead, FromTTree(..)
  , readEntry, readBranch, readBranchMaybe
  , pipeTTree, readTTreeEntry
  , MonadIO(..)
  , TTreeException(..)
  ) where

import           Control.Monad.Catch        as X
import           Control.Monad.IO.Class     as X (MonadIO (..))
import           Control.Monad.Reader       hiding (fail)
import           Control.Monad.State.Strict hiding (fail)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.TBranch
import           Data.TFile
import           Data.Typeable
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

ttree :: MonadIO m => TFile -> String -> m TTree
ttree f tn = liftIO $ do
    tp <- newForeignPtr_ =<< withCString tn (_ttree f)
    return $ TTree tp M.empty

isNullTree :: MonadIO m => TTree -> m Bool
isNullTree (TTree p _) = liftIO $ withForeignPtr p (return . (== nullPtr))

-- TODO
-- this really should be of type Int -> TTree -> (ExceptT TreeError m) a
-- but this is isomorphic, no?
type TreeRead m = ReaderT Int (StateT TTree m)

readEntry :: Monad m => TreeRead m Int
readEntry = ask

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


data TTreeException = EndOfTTree | TBranchReadFailure String
  deriving (Typeable, Show)

instance Exception TTreeException where


readBranch
  :: ( MonadIO m, MonadThrow m
     , Branchable a, Storable (HeapType a), Freeable (HeapType a) )
  => String -> TreeRead m a
readBranch s = do
  m <- readBranchMaybe s
  case m of
    Nothing -> do
      i <- ask
      throwM . TBranchReadFailure
        $ "unable to read branch " ++ s ++ " for event number " ++ show i
    Just x  -> return x


class FromTTree a where
    fromTTree :: (MonadIO m, MonadThrow m) => TreeRead m a


readTTreeEntry
  :: (MonadIO m, MonadState TTree m)
  => ReaderT Int m b -> Int -> m (Maybe b)
readTTreeEntry tr i = do
  t <- get
  n <- liftIO $ withForeignPtr (ttreePtr t) $ flip _ttreeLoadTree i
  if n >= 0 then Just <$> runReaderT tr i else return Nothing


pipeTTree
  :: (MonadIO m, MonadState TTree m)
  => ReaderT Int m b
  -> Pipe Int b m ()
pipeTTree f = do
  i <- await
  mx <- lift $ readTTreeEntry f i
  case mx of
    Just x -> do
      yield x
      pipeTTree f
    Nothing -> return ()
