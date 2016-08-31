{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.TTree ( ttree
                  , Branchable(..), readBranch
                  , TR, FromTTree(..)
                  , runTTree, runTTreeN, project
                  , MonadIO(..)
                  ) where

import Debug.Trace

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


-- void pointer
type VPtr = Ptr ()
type FVPtr = ForeignPtr ()

foreign import ccall "ttreeC.h ttree" _ttree
    :: CString -> CString -> IO VPtr
foreign import ccall "ttreeC.h ttreeGetBranchEntry" _ttreeGetBranchEntry
    :: VPtr -> CString -> Int -> Ptr a -> IO Int
foreign import ccall "ttreeC.h &ttreeFree" _ttreeFree
    :: FunPtr (Ptr a -> IO ())

foreign import ccall "ttreeC.h vectorSizeC" vectorSizeC
    :: VecPtr Char -> Int
foreign import ccall "ttreeC.h vectorDataC" vectorDataC
    :: VecPtr Char -> Ptr Char
foreign import ccall "ttreeC.h vectorFreeC" vectorFreeC
    :: VecPtr Char -> IO ()

foreign import ccall "ttreeC.h vectorSizeI" vectorSizeI
    :: VecPtr Int -> Int
foreign import ccall "ttreeC.h vectorDataI" vectorDataI
    :: VecPtr Int -> Ptr Int
foreign import ccall "ttreeC.h vectorFreeI" vectorFreeI
    :: VecPtr Int -> IO ()

foreign import ccall "ttreeC.h vectorSizeF" vectorSizeF
    :: VecPtr Float -> Int
foreign import ccall "ttreeC.h vectorDataF" vectorDataF
    :: VecPtr Float -> Ptr Float
foreign import ccall "ttreeC.h vectorFreeF" vectorFreeF
    :: VecPtr Float -> IO ()

foreign import ccall "ttreeC.h vectorSizeD" vectorSizeD
    :: VecPtr Double -> Int
foreign import ccall "ttreeC.h vectorDataD" vectorDataD
    :: VecPtr Double -> Ptr Double
foreign import ccall "ttreeC.h vectorFreeD" vectorFreeD
    :: VecPtr Double -> IO ()



type TTree = FVPtr


ttree :: String -> String -> IO TTree
ttree tn fn = do tn' <- newCString tn
                 fn' <- newCString fn
                 tp <- newForeignPtr _ttreeFree =<< _ttree tn' fn'
                 free tn' >> free fn'
                 return tp



class Branchable b where
    type HeapType b :: *
    fromB :: Ptr (HeapType b) -> IO b


instance Branchable Char where
    type HeapType Char = Char
    fromB = peek

instance Branchable Int where
    type HeapType Int = Int
    fromB = peek

instance Branchable CUInt where
    type HeapType CUInt = CUInt
    fromB = peek

instance Branchable CLong where
    type HeapType CLong = CLong
    fromB = peek

instance Branchable Float where
    type HeapType Float = Float
    fromB = peek

instance Branchable Double where
    type HeapType Double = Double
    fromB = peek


-- pointer to a c++ vector
newtype VecPtr a = VecPtr { vecPtr :: VPtr } deriving (Show, Storable)


class Storable a => Vecable a where
    sizeV :: VecPtr a -> Int
    dataV :: VecPtr a -> Ptr a
    freeV :: VecPtr a -> IO ()

    toV :: VecPtr a -> IO (VS.Vector a)
    toV vp = flip VS.unsafeFromForeignPtr0 (sizeV vp) <$> newForeignPtr_ (dataV vp)


instance Vecable Char where
    sizeV = vectorSizeC
    dataV = vectorDataC
    freeV = vectorFreeC . traceShowId

instance Vecable Int where
    sizeV = vectorSizeI
    dataV = vectorDataI
    freeV = vectorFreeI . traceShowId

instance Vecable Float where
    sizeV = vectorSizeF
    dataV = vectorDataF
    freeV = vectorFreeF . traceShowId

instance Vecable Double where
    sizeV = vectorSizeD
    dataV = vectorDataD
    freeV = vectorFreeD . traceShowId




instance Vecable a => Branchable (V.Vector a) where
    type HeapType (V.Vector a) = VecPtr a
    fromB vp = do p <- peek vp
                  v <- VS.convert <$> toV p
                  freeV p
                  return v

instance Vecable a => Branchable [a] where
    type HeapType [a] = VecPtr a
    fromB vp = do p <- peek vp
                  v <- VS.toList <$> toV p
                  freeV p
                  return v

instance Vecable a => Branchable (ZipList a) where
    type HeapType (ZipList a) = VecPtr a
    fromB = fmap ZipList <$> fromB


type TR m a = ReaderT (TTree, Int) (MaybeT m) a


readBranch :: (MonadIO m, Branchable a, Storable (HeapType a)) => String -> TR m a
readBranch s = do (cp, i) <- ask
                  traceShow ("trying to read branch" ++ s) $ return ()
                  bp <- liftIO $ newForeignPtr finalizerFree =<< calloc
                  n <- liftIO $ withCString s $ \s' -> withForeignPtr cp
                                              $ \cp' -> withForeignPtr bp
                                              $ \bp' -> _ttreeGetBranchEntry cp' s' i bp'

                  traceShow "branch address set." $ return ()

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
