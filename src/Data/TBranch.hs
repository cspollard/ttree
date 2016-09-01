{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Data.TBranch ( Branchable(..), Freeable(..) ) where

import Foreign hiding (void)
import Foreign.C.Types (CLong)

import Control.Monad ((>=>))

import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V

import Control.Applicative (ZipList(..))


import Data.STLVec

class Branchable b where
    type HeapType b :: *
    fromB :: Ptr (HeapType b) -> IO b


instance Branchable Char where
    type HeapType Char = Char
    fromB = peek

instance Branchable CLong where
    type HeapType CLong = CLong
    fromB = peek

instance Branchable Int where
    type HeapType Int = Int
    fromB = peek

instance Branchable Float where
    type HeapType Float = Float
    fromB = peek

instance Branchable Double where
    type HeapType Double = Double
    fromB = peek

instance Vecable a => Branchable (V.Vector a) where
    type HeapType (V.Vector a) = VecPtr a
    fromB = fmap VS.convert <$> (peek >=> toV)

instance Vecable a => Branchable [a] where
    type HeapType [a] = VecPtr a
    fromB = fmap VS.toList <$> (peek >=> toV)

instance Vecable a => Branchable (ZipList a) where
    type HeapType (ZipList a) = VecPtr a
    fromB = fmap ZipList <$> fromB


newtype VVector a = VVector { fromVVector :: V.Vector (V.Vector a) } deriving Show

instance Vecable a => Branchable (VVector a) where
    type HeapType (VVector a) = VecPtr (VecPtr a)
    fromB vvp = do vpp <- readVV vvp
                   vv <- VS.freeze . VS.MVector (sizeV vvp) =<< newForeignPtr vectorFreeP vpp
                   fmap VVector . mapM toV $ VS.convert vv


class Freeable a where
    free' :: FunPtr (Ptr a -> IO ())

instance Freeable Char where
    free' = finalizerFree

instance Freeable Int where
    free' = finalizerFree

instance Freeable CLong where
    free' = finalizerFree

instance Freeable Float where
    free' = finalizerFree

instance Freeable Double where
    free' = finalizerFree

instance Vecable a => Freeable (VecPtr a) where
    free' = freeV
