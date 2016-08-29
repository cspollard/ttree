{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.TTree ( ttree
                  , Branchable(..), readBranch
                  , TTreeRead, FromTTree(..)
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
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe

import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Control.Applicative (ZipList(..))


-- void pointer
type VPtr = Ptr ()
type FVPtr = ForeignPtr ()

foreign import ccall "ttreeC.h ttree" _ttree
    :: CString -> CString -> IO VPtr
foreign import ccall "ttreeC.h ttreeResetBranchAddresses" _ttreeResetBranchAddresses
    :: VPtr -> IO ()
foreign import ccall "ttreeC.h ttreeGetBranchEntry" _ttreeGetBranchEntry
    :: VPtr -> CString -> Int -> Ptr a -> IO Int
foreign import ccall "ttreeC.h &ttreeFree" _ttreeFree
    :: FunPtr (Ptr a -> IO ())

foreign import ccall "ttreeC.h vectorSizeC" vectorSizeC
    :: VecPtr Char -> Int
foreign import ccall "ttreeC.h vectorDataC" vectorDataC
    :: VecPtr Char -> Ptr Char
foreign import ccall "ttreeC.h vectorSizeI" vectorSizeI
    :: VecPtr Int -> Int
foreign import ccall "ttreeC.h vectorDataI" vectorDataI
    :: VecPtr Int -> Ptr Int
foreign import ccall "ttreeC.h vectorSizeF" vectorSizeF
    :: VecPtr Float -> Int
foreign import ccall "ttreeC.h vectorDataF" vectorDataF
    :: VecPtr Float -> Ptr Float
foreign import ccall "ttreeC.h vectorSizeD" vectorSizeD
    :: VecPtr Double -> Int
foreign import ccall "ttreeC.h vectorDataD" vectorDataD
    :: VecPtr Double -> Ptr Double


type TTree = FVPtr


ttree :: String -> String -> IO TTree
ttree tn fn = do tn' <- newCString tn
                 fn' <- newCString fn
                 tp <- newForeignPtr _ttreeFree =<< _ttree tn' fn'
                 free tn' >> free fn'
                 return tp



-- getEntry :: MonadIO m => a -> TTreeRead m (Maybe a)
-- getEntry x = do (cp, i, _) <- get
                -- n <- liftIO $ withForeignPtr cp $ flip _ttreeGetEntry i
                -- return $ if n > 0 then Just x else Nothing


class Branchable b where
    type HeapType b :: *
    fromBranch :: ForeignPtr (HeapType b) -> IO b

instance Branchable Char where
    type HeapType Char = Char
    fromBranch = flip withForeignPtr peek

instance Branchable Int where
    type HeapType Int = Int
    fromBranch = flip withForeignPtr peek

instance Branchable CUInt where
    type HeapType CUInt = CUInt
    fromBranch = flip withForeignPtr peek

instance Branchable CLong where
    type HeapType CLong = CLong
    fromBranch = flip withForeignPtr peek

instance Branchable Float where
    type HeapType Float = Float
    fromBranch = flip withForeignPtr peek

instance Branchable Double where
    type HeapType Double = Double
    fromBranch = flip withForeignPtr peek


-- pointer to a c++ vector
newtype VecPtr a = VecPtr VPtr deriving (Show, Storable)


class Storable a => Vecable a where
    sizeV :: VecPtr a -> Int
    dataV :: VecPtr a -> Ptr a

    -- faster for accessing just one item
    ixV :: MonadIO m => VecPtr a -> Int -> m a
    v `ixV` i = liftIO . peek $ advancePtr (dataV v) i

    -- faster for accessing all items together
    toV :: VecPtr a -> IO (VS.Vector a)
    toV vp = flip VS.unsafeFromForeignPtr0 (sizeV vp) <$> newForeignPtr_ (dataV vp)


instance Vecable Char where
    sizeV = vectorSizeC
    dataV = vectorDataC

instance Vecable Int where
    sizeV = vectorSizeI
    dataV = vectorDataI

instance Vecable Float where
    sizeV = vectorSizeF
    dataV = vectorDataF

instance Vecable Double where
    sizeV = vectorSizeD
    dataV = vectorDataD


instance Vecable a => Branchable (V.Vector a) where
    type HeapType (V.Vector a) = VecPtr a
    fromBranch = flip withForeignPtr $ peek >=> toV >=> return . VS.convert

instance Vecable a => Branchable [a] where
    type HeapType [a] = VecPtr a
    fromBranch = flip withForeignPtr $ peek >=> toV >=> return . VS.toList

instance Vecable a => Branchable (ZipList a) where
    type HeapType (ZipList a) = VecPtr a
    fromBranch = flip withForeignPtr $ peek >=> toV >=> return . ZipList . VS.toList


type TTreeRead m a = StateT (TTree, Int, HM.HashMap String FVPtr) (MaybeT m) a

readBranch :: (MonadIO m, Branchable a, Storable (HeapType a)) => String -> TTreeRead m a
readBranch s = do (tp, i, hm) <- get
                  case hm ^. at s of
                       Just p  -> liftIO . fromBranch . castForeignPtr $ p
                       Nothing -> do bp <- liftIO calloc
                                     n <- liftIO $ withCString s $ \s' -> withForeignPtr tp $ \tp' -> _ttreeGetBranchEntry tp' s' i bp
                                     p <- liftIO $ newForeignPtr finalizerFree bp
                                     if n <= 0
                                        then fail ("failed to read branch " ++ s)
                                        else do put (tp, i, hm & at s .~ Just (castForeignPtr p))
                                                liftIO $ fromBranch p


class FromTTree a where
    fromTTree :: MonadIO m => TTreeRead m a


runTTree :: MonadIO m => TTreeRead (ConduitM i o m) o -> TTree -> ConduitM i o m ()
runTTree f c = loop c 0 
    where loop c' i = do liftIO $ withForeignPtr c' _ttreeResetBranchAddresses
                         ms <- runMaybeT $ evalStateT f (c', i, HM.empty)
                         case ms of
                              Just x  -> yield x >> loop c' (i+1)
                              Nothing -> return ()


runTTreeN :: MonadIO m => Int -> TTreeRead (ConduitM i o m) o -> TTree -> ConduitM i o m ()
runTTreeN n f c = runTTree f c =$= takeC n


project :: (MonadIO m, FromTTree fc) => TTree -> ConduitM i fc m ()
project = runTTree fromTTree
