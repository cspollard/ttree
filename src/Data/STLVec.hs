{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.STLVec ( VecPtr(..), Vecable(..)
                   , VVecPtr(..), VVecable(..)
                   , VVector(..)
                   ) where

import Foreign hiding (void)

import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V

foreign import ccall "ttreeC.h vectorSizeC" vectorSizeC
    :: Ptr (Vec Char) -> Int
foreign import ccall "ttreeC.h vectorDataC" vectorDataC
    :: Ptr (Vec Char) -> Ptr Char
foreign import ccall "ttreeC.h &vectorFreeC" vectorFreeC
    :: FunPtr (Ptr (Vec Char) -> IO ())

foreign import ccall "ttreeC.h vectorSizeI" vectorSizeI
    :: Ptr (Vec Int) -> Int
foreign import ccall "ttreeC.h vectorDataI" vectorDataI
    :: Ptr (Vec Int) -> Ptr Int
foreign import ccall "ttreeC.h &vectorFreeI" vectorFreeI
    :: FunPtr (Ptr (Vec Int) -> IO ())

foreign import ccall "ttreeC.h vectorSizeF" vectorSizeF
    :: Ptr (Vec Float) -> Int
foreign import ccall "ttreeC.h vectorDataF" vectorDataF
    :: Ptr (Vec Float) -> Ptr Float
foreign import ccall "ttreeC.h &vectorFreeF" vectorFreeF
    :: FunPtr (Ptr (Vec Float) -> IO ())

foreign import ccall "ttreeC.h vectorSizeD" vectorSizeD
    :: Ptr (Vec Double) -> Int
foreign import ccall "ttreeC.h vectorDataD" vectorDataD
    :: Ptr (Vec Double) -> Ptr Double
foreign import ccall "ttreeC.h &vectorFreeD" vectorFreeD
    :: FunPtr (Ptr (Vec Double) -> IO ())

foreign import ccall "ttreeC.h vectorNewVD" vectorNewVD
    :: IO (Ptr (VVec Double))
foreign import ccall "ttreeC.h vectorSizeVD" vectorSizeVD
    :: Ptr (VVec Double) -> Int
foreign import ccall "ttreeC.h vectorDataVD" vectorDataVD
    :: Ptr (VVec Double) -> Ptr (Vec Double)
foreign import ccall "ttreeC.h vectorBPtrVD" vectorBPtrVD
    :: Ptr (VVec Double) -> Ptr ()
foreign import ccall "ttreeC.h &vectorFreeVD" vectorFreeVD
    :: FunPtr (Ptr (VVec Double) -> IO ())

instance Storable () where
    sizeOf _ = 0
    alignment _ = 1
    peek _ = return ()
    poke _ _ = return ()

-- pointer to a c++ vector<T>
newtype Vec a = Vec () deriving Storable

instance Vecable Char where
    sizeV = vectorSizeC
    dataV = vectorDataC
    freeV = vectorFreeC

instance Vecable Int where
    sizeV = vectorSizeI
    dataV = vectorDataI
    freeV = vectorFreeI

instance Vecable Float where
    sizeV = vectorSizeF
    dataV = vectorDataF
    freeV = vectorFreeF

instance Vecable Double where
    sizeV = vectorSizeD
    dataV = vectorDataD
    freeV = vectorFreeD


-- wrapper around a c++ vector<vector<T> >
newtype VVec a = VVec ((Ptr ()) deriving Storable
newtype VVector a = VVector { fromVVector :: V.Vector (V.Vector a) } deriving Show

class Vecable a => VVecable a where
    newVV :: IO (Ptr (VVec a))
    sizeVV :: Ptr (VVec a) -> Int
    dataVV :: Ptr (VVec a) -> Ptr (Vec a)
    bptrVV :: Ptr (VVec a) -> Ptr ()
    freeVV :: FunPtr (Ptr (VVec a) -> IO ())

    toVV :: Ptr (VVec a) -> IO (VVector a)
    toVV vp = do vv <- VS.freeze . VS.MVector (sizeVV vp) =<< newForeignPtr_ (dataVV vp)
                 fmap VVector . mapM toV $ VS.convert vv


instance VVecable Double where
    newVV = vectorNewVD
    sizeVV = vectorSizeVD
    dataVV = vectorDataVD
    bptrVV = vectorBPtrVD
    freeVV = vectorFreeVD
