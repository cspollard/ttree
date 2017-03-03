{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.TBranch
  ( Branchable(..), Freeable(..), VVector(..)
  , CInt(..), CChar(..), CLong(..)
  ) where

import           Control.Applicative  (ZipList (..))
import           Control.Monad        ((>=>))
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS
import           Foreign              hiding (void)
import           Foreign.C.Types      (CChar (..), CInt (..), CLong (..))

import           Data.STLVec

class Branchable b where
    type HeapType b :: *
    fromB :: Ptr (HeapType b) -> IO b


instance Branchable CChar where
    type HeapType CChar = CChar
    fromB = peek

instance Branchable CInt where
    type HeapType CInt = CInt
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

instance Vecable a => Branchable (V.Vector a) where
    type HeapType (V.Vector a) = VecPtr a
    fromB = fmap VS.convert . (peek >=> toV)

instance Vecable a => Branchable [a] where
    type HeapType [a] = VecPtr a
    fromB = fmap V.toList . fromB

instance Vecable a => Branchable (ZipList a) where
    type HeapType (ZipList a) = VecPtr a
    fromB = fmap ZipList . fromB


newtype VVector a = VVector { fromVVector :: V.Vector (V.Vector a) } deriving Show

instance VVecable a => Branchable (VVector a) where
    type HeapType (VVector a) = VVecPtr a
    fromB vvp = do
      vpp <- (peek >=> readVV) vvp

      -- immediately freeze vpp and free it
      vv <- VS.freeze . VS.MVector (fromEnum $ sizeV vpp)
              =<< newForeignPtr_ (dataV vpp)

      p' <- malloc
      poke p' vpp
      finalizeForeignPtr =<< newForeignPtr freeV p'

      fmap VVector . mapM (fmap VS.convert . toV) $ VS.convert vv


class Freeable a where
    free' :: FunPtr (Ptr a -> IO ())

instance Freeable CChar where
    free' = finalizerFree

instance Freeable CInt where
    free' = finalizerFree

instance Freeable CLong where
    free' = finalizerFree

instance Freeable Float where
    free' = finalizerFree

instance Freeable Double where
    free' = finalizerFree

instance Vecable a => Freeable (VecPtr a) where
    free' = freeV

instance VVecable a => Freeable (VVecPtr a) where
    free' = freeVV
