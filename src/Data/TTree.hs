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
  , readBranch, readBranchMaybe
  , TreeRead, FromTTree(..)
  , readEntry, alignThesePipes, alignPipesBy
  , runTTree, produceTTree
  , MonadIO(..)
  , TTreeException(..)
  ) where

import           Control.Monad.Catch        as X
import           Control.Monad.IO.Class     as X (MonadIO (..))
import           Control.Monad.Reader       hiding (fail)
import           Control.Monad.State.Strict hiding (fail)
import           Control.Monad.Trans        (lift)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Semigroup
import           Data.TBranch
import           Data.TFile
import           Data.These
import           Data.Typeable
import           Foreign                    hiding (void)
import           Foreign.C.String
import           Pipes
import           Pipes.Lift
import qualified Pipes.Prelude              as P
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


runTreeRead
  :: (MonadThrow m, MonadIO m, MonadState TTree m)
  => ReaderT Int m b -> Int -> m b
runTreeRead tr i = do
    t <- get
    n <- liftIO $ withForeignPtr (ttreePtr t) $ flip _ttreeLoadTree i
    if n >= 0 then runReaderT tr i else throwM EndOfTTree


runTTree
  :: (MonadIO m, MonadThrow m)
  => TreeRead m b -> TTree -> Producer' b m ()
runTTree f = produceTTree f (each [0..])


produceTTree
  :: (MonadThrow m, MonadIO m)
  => TreeRead m b
  -> Producer' Int (StateT TTree m) r
  -> TTree
  -> Producer' b m r
produceTTree f p t = evalStateP t $ for p (yield <=< lift . runTreeRead f)


alignThesePipes
  :: (Monad m, Ord a)
  => (t -> a)
  -> (t' -> a)
  -> Producer t m ()
  -> Producer t' m ()
  -> Producer (a, These t t') m ()
alignThesePipes comp comp' = go
  where
    go p1 p2 = do
      e1 <- lift $ next p1
      e2 <- lift $ next p2
      case (e1, e2) of
        (Left (), Left ()) -> return ()
        (Left (), Right (x2, p2')) -> yield (comp' x2, That x2) >> go p1 p2'
        (Right (x1, p1'), Left ()) -> yield (comp x1, This x1) >> go p1' p2
        (Right (x1, p1'), Right (x2, p2')) ->
          let a1 = comp x1
              a2 = comp' x2
          in case a1 `compare` a2 of
            LT -> yield (a1, This x1) >> go p1' p2
            GT -> yield (a2, That x2) >> go p1 p2'
            EQ -> yield (a1, These x1 x2) >> go p1' p2'



alignPipesBy
  :: (Monad m, Traversable f, Ord a)
  => (t -> a)
  -> f (Producer t m ())
  -> Producer (a, f (Maybe t)) m ()
alignPipesBy comp ps = go ps'
  where
    ps' = (>-> P.map (\t -> (t, comp t))) <$> ps

    go ps'' = do
      ftps <- lift $ traverse (\p -> (p,) <$> next p) ps''
      let mxmin =
            getOption $ foldMap (g . snd) ftps
      case mxmin of
        -- all producers are empty
        Nothing   -> return ()
        Just (Min xmin) -> do
          let tmp = h xmin <$> ftps
              ps''' = fst <$> tmp
              xs = snd <$> tmp
          yield (xmin, xs) >> go ps'''

    g :: Either () ((t, a), t1) -> Option (Min a)
    g (Left ())           = Option Nothing
    g (Right ((_, x), _)) = Option . Just $ Min x

    h _ (p, Left ()) = (p, Nothing)
    h xmin (p, Right ((t, x), p')) =
      if x == xmin
        then (p', Just t)
        else (p, Nothing)
