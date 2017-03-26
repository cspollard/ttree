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
  , TR, TreeError(..), FromTTree(..)
  , produceTTree, pipeTTree, foldTTree, foldMTTree, runTR
  , MonadIO(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Trans        (lift)
import           Data.Bifunctor             (first, second)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Foreign                    hiding (void)
import           Foreign.C.String
import           Pipes
import qualified Pipes.Prelude              as P

import           Data.TBranch
import           Data.TFile


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

type TR m = StateT (TTree, Int) (ExceptT TreeError m)

data TreeError = BranchError | EndOfTree
  deriving Show

-- note: this assumes that once a branch has been requested
-- that we want to load it for *EVERY EVENT*
readBranchMaybe
  :: (MonadIO m, Branchable a, Storable (HeapType a), Freeable (HeapType a))
  => String -> TR m (Maybe a)
readBranchMaybe s = do
  (t, i) <- get
  case s `M.lookup` ttreeBranches t of
    -- we've already read this branch at least once: don't alloc a
    -- new pointer
    Just p  -> fmap Just . liftIO $ withForeignPtr (castForeignPtr p) fromB

    -- this is the first time we've accessed this branch: alloc a
    -- new pointer
    Nothing -> do
      p <- liftIO $ newForeignPtr_ =<< calloc
      modify . first . const
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

-- fail if the branch doesn't exist or is unreadable.
readBranch
  :: (MonadIO m, Branchable a, Storable (HeapType a), Freeable (HeapType a))
  => String -> TR m a
readBranch s = do
  m <- readBranchMaybe s
  case m of
    Nothing -> lift $ throwError BranchError
    Just x  -> return x


class FromTTree a where
    fromTTree :: MonadIO m => TR m a

loadTree
  :: MonadIO m
  => TR m a -> TR m a
loadTree f = do
  (t, i) <- get
  n <- liftIO $ withForeignPtr (ttreePtr t) $ flip _ttreeLoadTree i
  if n < 0
    then throwError EndOfTree
    else f

runTR :: Monad m => TTree -> TR m a -> m (Either TreeError a)
runTR t = runExceptT . flip evalStateT (t, 0)

pipeTTree :: MonadIO m => TR m a -> Pipe Int a (TR m) ()
pipeTTree f = go
  where
    go =
      let f' = loadTree f
      in do
        i <- await
        modify (second $ const i)
        flip catchError ce $ yield =<< lift f'
        go

    ce EndOfTree = void (liftIO (putStrLn "end of tree"))
    ce e         = throwError e


produceTTree :: MonadIO m => TR m a -> Producer a (TR m) ()
produceTTree f = ints >-> pipeTTree f
  where ints = each [0..]

foldMTTree
  :: Monad m
  => (x -> a -> TR m x)
  -> TR m x
  -> (x -> TR m b)
  -> TTree
  -> Producer a (TR m) ()
  -> m (Either TreeError b)
foldMTTree comb start done t prod =
  runTR t $ P.foldM comb start done prod

foldTTree
  :: Monad m
  => (x -> a -> x)
  -> x
  -> (x -> b)
  -> TTree
  -> Producer a (TR m) ()
  -> m (Either TreeError b)
foldTTree comb start done t prod =
  runTR t $ P.fold comb start done prod


-- foldlT :: Monad m => (b -> o -> m b) -> m b -> MachineT m k o -> m b
-- foldlT f = go
--     where
--       go mb m = do
--         step <- runMachineT m
--         case step of
--           Stop         -> mb
--           Yield o m'   -> go (flip f o =<< mb) m'
--           Await _ _ m' -> go mb m'
--
--
-- foldTTree :: (MonadIO m) => (b -> a -> TR m b) -> TR m a -> TTree -> b -> m b
-- foldTTree comb f t x = sourceTTree f t
--

-- runTTree :: MonadIO m => (o -> TR m o) -> o -> TTree -> m o
-- runTTree f o c = loop o c 0
--   where
--     loop o' c' i = do
--       n <- liftIO $ withForeignPtr (ttreePtr c') $ flip _ttreeLoadTree i
--       if n < 0
--         then return o'
--         else do
--           ms <- runExceptT $ runRWST (f o') i c'
--           case ms of
--             Left s            -> error s
--             Right (x, c'', _) -> loop x c'' (i+1)
--
--
-- -- TODO
-- -- I think the following could be written in terms of the previous.
-- runTTreeLWithLog :: MonadIO m => TR m o -> TTree -> ListT (WriterT [T.Text] m) o
-- runTTreeLWithLog f c = loop c 0
--   where
--     loop c' i = ListT . WriterT $ do
--       n <- liftIO $ withForeignPtr (ttreePtr c') $ flip _ttreeLoadTree i
--       if n < 0
--         then return (Nil, mempty)
--         else do
--           ms <- runExceptT $ runRWST f i c'
--           case ms of
--             Left s            -> error s
--             Right (x, c'', s) -> return . (,s) .  Cons x $ loop c'' (i+1)
--
--
-- dropWriter :: Monad m => ListT (WriterT x m) o -> ListT m o
-- dropWriter l = ListT $ do
--   x <- fmap fst . runWriterT $ next l
--   case x of
--     Cons y l' -> return . Cons y $ dropWriter l'
--     Nil       -> return Nil
--
-- printWriter :: (MonadIO m, Show s) => ListT (WriterT [s] m) o -> ListT m o
-- printWriter l = ListT $ do
--   (x, s) <- runWriterT $ next l
--   liftIO $ putStrLn "branch reads:"
--   _ <- liftIO $ traverse print s
--   case x of
--     Cons y l' -> return . Cons y $ printWriter l'
--     Nil       -> return Nil
--
-- runTTreeLDebug :: MonadIO m => TR m o -> TTree -> ListT m o
-- runTTreeLDebug f t = printWriter $ runTTreeLWithLog f t
--
-- runTTreeL :: MonadIO m => TR m o -> TTree -> ListT m o
-- runTTreeL f t = dropWriter $ runTTreeLWithLog f t
--
--
-- runTTreeLN :: MonadIO m => Int -> TR m o -> TTree -> ListT m o
-- runTTreeLN n f c = L.take n $ runTTreeL f c
--
-- project :: (MonadIO m, FromTTree fc) => TTree -> ListT m fc
-- project = runTTreeL fromTTree
