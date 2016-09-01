{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Data.STLVec where

import Foreign hiding (void)

import qualified Data.Vector.Storable as VS

foreign import ccall "ttreeC.h vectorSizeC" vectorSizeC
    :: VecPtr Char -> Int
foreign import ccall "ttreeC.h vectorDataC" vectorDataC
    :: VecPtr Char -> Ptr Char
foreign import ccall "ttreeC.h &vectorFreeC" vectorFreeC
    :: FunPtr (VecPtr Char -> IO ())

foreign import ccall "ttreeC.h vectorSizeI" vectorSizeI
    :: VecPtr Int -> Int
foreign import ccall "ttreeC.h vectorDataI" vectorDataI
    :: VecPtr Int -> Ptr Int
foreign import ccall "ttreeC.h &vectorFreeI" vectorFreeI
    :: FunPtr (VecPtr Int -> IO ())

foreign import ccall "ttreeC.h vectorSizeF" vectorSizeF
    :: VecPtr Float -> Int
foreign import ccall "ttreeC.h vectorDataF" vectorDataF
    :: VecPtr Float -> Ptr Float
foreign import ccall "ttreeC.h &vectorFreeF" vectorFreeF
    :: FunPtr (VecPtr Float -> IO ())

foreign import ccall "ttreeC.h vectorSizeD" vectorSizeD
    :: VecPtr Double -> Int
foreign import ccall "ttreeC.h vectorDataD" vectorDataD
    :: VecPtr Double -> Ptr Double
foreign import ccall "ttreeC.h &vectorFreeD" vectorFreeD
    :: FunPtr (VecPtr Double -> IO ())

foreign import ccall "ttreeC.h vectorSizeP" vectorSizeP
    :: VecPtr VPtr -> Int
foreign import ccall "ttreeC.h vectorDataP" vectorDataP
    :: VecPtr VPtr -> Ptr VPtr
foreign import ccall "ttreeC.h &vectorFreeP" vectorFreeP
    :: FunPtr (VecPtr VPtr -> IO ())

foreign import ccall "ttreeC.h vectorReadV" vectorReadV
    :: VecPtr VPtr -> VecPtr VPtr


-- pointer to a c++ vector
newtype VecPtr a = VecPtr { vecPtr :: Ptr () } deriving (Show, Storable)
newtype VVecPtr a = VVecPtr { vvecPtr :: Ptr () } deriving (Show, Storable)

castVecPtr :: VecPtr a -> VecPtr b
castVecPtr (VecPtr p) = VecPtr p

newtype VPtr = VPtr { vptr :: Ptr () } deriving (Show, Storable)

class (Show a, Storable a) => Vecable a where
    sizeV :: VecPtr a -> Int
    dataV :: VecPtr a -> Ptr a
    freeV :: FunPtr (VecPtr a -> IO ())

    toV :: VecPtr a -> IO (VS.Vector a)
    toV vp = VS.freeze . VS.MVector (sizeV vp) =<< newForeignPtr_ (dataV vp)


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

instance Vecable VPtr where
    sizeV = vectorSizeP
    dataV = vectorDataP
    freeV = vectorFreeP

readVV :: VecPtr VPtr -> VecPtr (VecPtr a)
readVV = castVecPtr . vectorReadV
