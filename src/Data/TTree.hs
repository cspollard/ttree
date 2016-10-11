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

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

import Data.TBranch


-- void pointer
type VPtr = Ptr ()
type FVPtr = ForeignPtr ()

foreign import ccall "ttreeC.h ttree" _ttree
    :: CString -> CString -> IO VPtr
foreign import ccall "ttreeC.h ttreeGetBranchEntry" _ttreeGetBranchEntry
    :: VPtr -> CString -> Int -> Ptr a -> IO Int
foreign import ccall "ttreeC.h ttreeResetBranchAddress" _ttreeResetBranchAddress
    :: VPtr -> CString -> IO ()
foreign import ccall "ttreeC.h &ttreeFree" _ttreeFree
    :: FunPtr (Ptr a -> IO ())


type TTree = FVPtr

ttree :: String -> String -> IO TTree
ttree tn fn = do tn' <- newCString tn
                 fn' <- newCString fn
                 tp <- newForeignPtr _ttreeFree =<< _ttree tn' fn'
                 free tn' >> free fn'
                 return tp


type TR m a = ReaderT (TTree, Int) (MaybeT m) a

readBranch :: (MonadIO m, Branchable a, Storable (HeapType a), Freeable (HeapType a))
           => String -> TR m a
readBranch s = do (tp, i) <- ask
                  bp <- liftIO $ newForeignPtr free' =<< calloc
                  n <- liftIO $ withCString s $ \s' -> withForeignPtr tp
                                              $ \tp' -> withForeignPtr bp
                                              $ \bp' -> _ttreeGetBranchEntry tp' s' i bp'

                  liftIO $ withCString s $ \s' -> withForeignPtr tp
                                         $ \tp' -> _ttreeResetBranchAddress tp' s'

                  if n <= 0
                     then fail $ "failed to read branch " ++ s
                     else liftIO $ withForeignPtr bp fromB


class FromTTree a where
    fromTTree :: MonadIO m => TR m a


runTTree :: MonadIO m => TR m o -> TTree -> ListT m o
runTTree f c = loop c 0
    where
        loop c' i = ListT $
            do
                ms <- runMaybeT $ runReaderT f (c', i)
                case ms of
                    Nothing -> return Nil
                    Just x  -> return . Cons x $ loop c' (i+1)


runTTreeN :: MonadIO m => Int -> TR m o -> TTree -> ListT m o
runTTreeN n f c = L.take n $ runTTree f c


project :: (MonadIO m, FromTTree fc) => TTree -> ListT m fc
project = runTTree fromTTree
