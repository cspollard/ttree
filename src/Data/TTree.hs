{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.TTree ( ttree, TTree, isNullTree
                  , module Data.TBranch
                  , readBranch
                  , TR, FromTTree(..)
                  , runTTree, runTTreeL, runTTreeLN, project
                  , MonadIO(..)
                  ) where

import List.Transformer (ListT(..), Step(..), MonadIO(..))
import qualified List.Transformer as L

import Foreign hiding (void)
import Foreign.C.String

import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.TBranch
import Data.TFile


-- void pointer
type VPtr = Ptr ()
type FVPtr = ForeignPtr ()

foreign import ccall "ttreeC.h ttree" _ttree
    :: TFile -> CString -> IO VPtr
foreign import ccall "ttreeC.h ttreeLoadTree" _ttreeLoadTree
    :: VPtr -> Int -> IO CLong
foreign import ccall "ttreeC.h ttreeGetBranchEntry" _ttreeGetBranchEntry
    :: VPtr -> CString -> Int -> Ptr a -> IO Int


data TTree =
    TTree
        { ttreePtr :: FVPtr
        , ttreeBranches :: Map String FVPtr
        }

ttree :: TFile -> String -> IO TTree
ttree f tn = do
    tp <- newForeignPtr_ =<< withCString tn (_ttree f)
    return $ TTree tp M.empty

isNullTree :: TTree -> IO Bool
isNullTree (TTree p _) = withForeignPtr p (return . (== nullPtr))

type TR m a = RWST Int () TTree (ExceptT String m) a

-- note: this assumes that once a branch has been requested
-- that we want to load it for *EVERY EVENT*
readBranch :: (MonadIO m, Branchable a, Storable (HeapType a), Freeable (HeapType a))
           => String -> TR m a
readBranch s = do
    t <- get
    i <- ask
    case s `M.lookup` ttreeBranches t of
        -- we've already read this branch at least once: don't alloc a
        -- new pointer
        Just p  -> liftIO $ withForeignPtr (castForeignPtr p) fromB

        -- this is the first time we've accessed this branch: alloc a
        -- new pointer
        Nothing -> do
            p <- liftIO $ newForeignPtr free' =<< calloc
            put $ t { ttreeBranches = M.insert s (castForeignPtr p) (ttreeBranches t) }
            n <- liftIO $ withCString s
                $ \s' -> withForeignPtr (ttreePtr t)
                $ \tp' -> withForeignPtr p
                $ \p' -> _ttreeGetBranchEntry tp' s' i p'

            if n <= 0
               then lift . throwE $ "failed to read branch " ++ s
               else liftIO $ withForeignPtr p fromB


class FromTTree a where
    fromTTree :: MonadIO m => TR m a


runTTree :: MonadIO m => (o -> TR m o) -> o -> TTree -> m o
runTTree f o c = loop o c 0
    where
        loop o' c' i = do
            n <- liftIO $ withForeignPtr (ttreePtr c') $ flip _ttreeLoadTree i
            if n < 0
                then return o'
                else do
                    ms <- runExceptT $ runRWST (f o') i c'
                    case ms of
                        Left s -> error s
                        Right (x, c'', _) -> loop x c'' (i+1)


-- TODO
-- I think the following could be written in terms of the previous.
runTTreeL :: MonadIO m => TR m o -> TTree -> ListT m o
runTTreeL f c = loop c 0
    where
        loop c' i = ListT $ do
            n <- liftIO $ withForeignPtr (ttreePtr c') $ flip _ttreeLoadTree i
            if n < 0
                then return Nil
                else do
                    ms <- runExceptT $ runRWST f i c'
                    case ms of
                        Left s -> error s
                        Right (x, c'', _) -> return . Cons x $ loop c'' (i+1)


runTTreeLN :: MonadIO m => Int -> TR m o -> TTree -> ListT m o
runTTreeLN n f c = L.take n $ runTTreeL f c

project :: (MonadIO m, FromTTree fc) => TTree -> ListT m fc
project = runTTreeL fromTTree
