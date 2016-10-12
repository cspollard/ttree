{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.TTree ( ttree, TTree
                  , module Data.TBranch
                  , readBranch
                  , TR, FromTTree(..)
                  , runTTree, runTTreeN, project
                  , MonadIO(..)
                  ) where

import List.Transformer (ListT(..), Step(..), MonadIO(..))
import qualified List.Transformer as L

import Foreign hiding (void)
import Foreign.C.String

import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Maybe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.TBranch


-- void pointer
type VPtr = Ptr ()
type FVPtr = ForeignPtr ()

foreign import ccall "ttreeC.h ttree" _ttree
    :: CString -> CString -> IO VPtr
foreign import ccall "ttreeC.h ttreeLoadTree" _ttreeLoadTree
    :: VPtr -> Int -> IO CLong
foreign import ccall "ttreeC.h ttreeGetBranchEntry" _ttreeGetBranchEntry
    :: VPtr -> CString -> Int -> Ptr a -> IO Int
foreign import ccall "ttreeC.h &ttreeFree" _ttreeFree
    :: FunPtr (Ptr a -> IO ())


data TTree =
    TTree
        { ttreePtr :: FVPtr
        , ttreeBranches :: Map String FVPtr
        }

ttree :: String -> String -> IO TTree
ttree tn fn = do tn' <- newCString tn
                 fn' <- newCString fn
                 tp <- newForeignPtr _ttreeFree =<< _ttree tn' fn'
                 free tn' >> free fn'
                 return $ TTree tp M.empty


type TR m a = RWST Int () TTree (MaybeT m) a

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
               then fail $ "failed to read branch " ++ s
               else liftIO $ withForeignPtr p fromB


class FromTTree a where
    fromTTree :: MonadIO m => TR m a


runTTree :: MonadIO m => TR m o -> TTree -> ListT m o
runTTree f c = loop c 0
    where
        loop c' i = ListT $ do
            n <- liftIO $ withForeignPtr (ttreePtr c') $ flip _ttreeLoadTree i
            if n < 0
                then return Nil
                else do
                    ms <- runMaybeT $ runRWST f i c'
                    case ms of
                        Nothing -> return Nil
                        Just (x, c'', _) -> return . Cons x $ loop c'' (i+1)


runTTreeN :: MonadIO m => Int -> TR m o -> TTree -> ListT m o
runTTreeN n f c = L.take n $ runTTree f c


project :: (MonadIO m, FromTTree fc) => TTree -> ListT m fc
project = runTTree fromTTree
