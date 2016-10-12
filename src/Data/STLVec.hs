{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Data.STLVec where

import Foreign hiding (void)

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Foreign.C.Types (CInt(..), CChar(..))

foreign import ccall "ttreeC.h vectorSizeC" vectorSizeC
    :: VecPtr CChar -> CInt
foreign import ccall "ttreeC.h vectorDataC" vectorDataC
    :: VecPtr CChar -> Ptr CChar
foreign import ccall "ttreeC.h &vectorFreeC" vectorFreeC
    :: FunPtr (Ptr (VecPtr CChar) -> IO ())

foreign import ccall "ttreeC.h vectorSizeI" vectorSizeI
    :: VecPtr CInt -> CInt
foreign import ccall "ttreeC.h vectorDataI" vectorDataI
    :: VecPtr CInt -> Ptr CInt
foreign import ccall "ttreeC.h &vectorFreeI" vectorFreeI
    :: FunPtr (Ptr (VecPtr CInt) -> IO ())

foreign import ccall "ttreeC.h vectorSizeF" vectorSizeF
    :: VecPtr Float -> CInt
foreign import ccall "ttreeC.h vectorDataF" vectorDataF
    :: VecPtr Float -> Ptr Float
foreign import ccall "ttreeC.h &vectorFreeF" vectorFreeF
    :: FunPtr (Ptr (VecPtr Float) -> IO ())

foreign import ccall "ttreeC.h vectorSizeD" vectorSizeD
    :: VecPtr Double -> CInt
foreign import ccall "ttreeC.h vectorDataD" vectorDataD
    :: VecPtr Double -> Ptr Double
foreign import ccall "ttreeC.h &vectorFreeD" vectorFreeD
    :: FunPtr (Ptr (VecPtr Double) -> IO ())

foreign import ccall "ttreeC.h vectorSizeP" vectorSizeP
    :: VecPtr (VecPtr a) -> CInt
foreign import ccall "ttreeC.h vectorDataP" vectorDataP
    :: VecPtr (VecPtr a) -> Ptr (VecPtr a)
foreign import ccall "ttreeC.h &vectorFreeP" vectorFreeP
    :: FunPtr (Ptr (VecPtr (VecPtr a)) -> IO ())

foreign import ccall "ttreeC.h vvReadC" vvReadC
    :: VVecPtr CChar -> IO (VecPtr (VecPtr CChar))
foreign import ccall "ttreeC.h &vvFreeC" vvFreeC
    :: FunPtr (Ptr (VVecPtr CChar) -> IO ())

foreign import ccall "ttreeC.h vvReadI" vvReadI
    :: VVecPtr CInt -> IO (VecPtr (VecPtr CInt))
foreign import ccall "ttreeC.h &vvFreeI" vvFreeI
    :: FunPtr (Ptr (VVecPtr CInt) -> IO ())

foreign import ccall "ttreeC.h vvReadF" vvReadF
    :: VVecPtr Float -> IO (VecPtr (VecPtr Float))
foreign import ccall "ttreeC.h &vvFreeF" vvFreeF
    :: FunPtr (Ptr (VVecPtr Float) -> IO ())

foreign import ccall "ttreeC.h vvReadD" vvReadD
    :: VVecPtr Double -> IO (VecPtr (VecPtr Double))
foreign import ccall "ttreeC.h &vvFreeD" vvFreeD
    :: FunPtr (Ptr (VVecPtr Double) -> IO ())


-- pointer to a c++ vector
newtype VecPtr a = VecPtr { vecPtr :: Ptr () } deriving (Show, Storable)
newtype VVecPtr a = VVecPtr { vvecPtr :: Ptr () } deriving (Show, Storable)

castVecPtr :: VecPtr a -> VecPtr b
castVecPtr (VecPtr p) = VecPtr p

newtype VPtr = VPtr { vptr :: Ptr () } deriving (Show, Storable)


class Storable a => Vecable a where
    sizeV :: VecPtr a -> CInt
    dataV :: VecPtr a -> Ptr a
    freeV :: FunPtr (Ptr (VecPtr a) -> IO ())

    toV :: VecPtr a -> IO (Vector a)
    toV vp = VS.freeze . VS.MVector (fromEnum $ sizeV vp) =<< newForeignPtr_ (dataV vp)


instance Vecable CChar where
    sizeV = vectorSizeC
    dataV = vectorDataC
    freeV = vectorFreeC

instance Vecable CInt where
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

instance Vecable (VecPtr a) where
    sizeV = vectorSizeP
    dataV = vectorDataP
    freeV = vectorFreeP


class Vecable a => VVecable a where
    readVV :: VVecPtr a -> IO (VecPtr (VecPtr a))
    freeVV :: FunPtr (Ptr (VVecPtr a) -> IO ())


instance VVecable CChar where
    readVV = vvReadC
    freeVV = vvFreeC

instance VVecable CInt where
    readVV = vvReadI
    freeVV = vvFreeI

instance VVecable Float where
    readVV = vvReadF
    freeVV = vvFreeF

instance VVecable Double where
    readVV = vvReadD
    freeVV = vvFreeD
