{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.TTree ( ttree
                  , Branchable(..), readBranch
                  , TR, FromTTree(..)
                  , runTTree, runTTreeN, project
                  , MonadIO(..)
                  ) where

import Conduit

import Control.Lens
import qualified Data.HashMap.Strict as HM

import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String

import Control.Monad ((>=>))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Control.Applicative (ZipList(..))

import Data.STLVec
import Data.TBranch


-- void pointer
type VPtr = Ptr ()
type FVPtr = ForeignPtr ()

foreign import ccall "ttreeC.h ttree" _ttree
    :: CString -> CString -> IO VPtr
foreign import ccall "ttreeC.h ttreeGetBranchEntry" _ttreeGetBranchEntry
    :: VPtr -> CString -> Int -> Ptr a -> IO Int
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
readBranch s = do (cp, i) <- ask
                  bp <- liftIO $ newForeignPtr free' =<< calloc
                  n <- liftIO $ withCString s $ \s' -> withForeignPtr cp
                                              $ \cp' -> withForeignPtr bp
                                              $ \bp' -> _ttreeGetBranchEntry cp' s' i bp'

                  if n <= 0
                     then fail $ "failed to read branch " ++ s
                     else liftIO $ withForeignPtr bp fromB


class FromTTree a where
    fromTTree :: MonadIO m => TR m a


runTTree :: MonadIO m => TR (ConduitM i o m) o -> TTree -> ConduitM i o m ()
runTTree f c = loop c 0 
    where loop c' i = do ms <- runMaybeT $ runReaderT f (c', i)
                         case ms of
                              Just x  -> yield x >> loop c' (i+1)
                              Nothing -> return ()


runTTreeN :: MonadIO m => Int -> TR (ConduitM i o m) o -> TTree -> ConduitM i o m ()
runTTreeN n f c = runTTree f c =$= takeC n


project :: (MonadIO m, FromTTree fc) => TTree -> ConduitM i fc m ()
project = runTTree fromTTree
