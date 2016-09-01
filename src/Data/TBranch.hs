{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Data.TBranch ( Branchable(..), FPtrable(..) ) where

import Foreign hiding (void)
import Foreign.C.Types (CLong)

import Control.Monad ((>=>))

import Data.Vector

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

instance Vecable a => Branchable (Vector a) where
    type HeapType (Vector a) = VecPtr a
    fromB = peek >=> toV

instance Vecable a => Branchable [a] where
    type HeapType [a] = VecPtr a
    fromB = fmap toList <$> (peek >=> toV)

instance Vecable a => Branchable (ZipList a) where
    type HeapType (ZipList a) = VecPtr a
    fromB = fmap ZipList <$> fromB

instance VVecable a => Branchable (VVector a) where
    type HeapType (VVector a) = VVec a
    fromB = peek >=> toVV


class FPtrable a where
    -- the handle on this object
    fptr :: IO (ForeignPtr a)
    -- the actual branch pointer that should be loaded
    bptr :: ForeignPtr a -> IO (ForeignPtr ())


instance FPtrable Char where
    fptr = newForeignPtr finalizerFree =<< calloc
    bptr = return . castForeignPtr

instance FPtrable Int where
    fptr = newForeignPtr finalizerFree =<< calloc
    bptr = return . castForeignPtr

instance FPtrable CLong where
    fptr = newForeignPtr finalizerFree =<< calloc
    bptr = return . castForeignPtr

instance FPtrable Float where
    fptr = newForeignPtr finalizerFree =<< calloc
    bptr = return . castForeignPtr

instance FPtrable Double where
    fptr = newForeignPtr finalizerFree =<< calloc
    bptr = return . castForeignPtr

instance Vecable a => FPtrable (VecPtr a) where
    fptr = newForeignPtr freeV =<< calloc
    bptr = return . castForeignPtr

instance VVecable a => FPtrable (VVecPtr a) where
    fptr = newForeignPtr freeVV =<< (vvecPtr <$> newVV)
    bptr vv = newForeignPtr_ =<< withForeignPtr vv (\p -> bptrVV <$> peek p)
