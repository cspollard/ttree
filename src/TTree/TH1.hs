{-# LANGUAGE ForeignFunctionInterface #-}

module TTree.TH1 where

import           Foreign         hiding (void)

import           Foreign.C.Types (CUInt (..))

type TH1D = Ptr ()
type TH1F = Ptr ()

foreign import ccall "th1C.h nbinsD" nbinsd
    :: TH1D -> IO CUInt
foreign import ccall "th1C.h binedgeD" binedged
    :: TH1D -> CUInt -> IO Double
foreign import ccall "th1C.h entryD" entryd
    :: TH1D -> CUInt -> IO Double
foreign import ccall "th1C.h uncertD" uncertd
    :: TH1D -> CUInt -> IO Double

foreign import ccall "th1C.h nbinsF" nbinsf
    :: TH1F -> IO CUInt
foreign import ccall "th1C.h binedgeF" binedgef
    :: TH1F -> CUInt -> IO Double
foreign import ccall "th1C.h entryF" entryf
    :: TH1F -> CUInt -> IO Float
foreign import ccall "th1C.h uncertF" uncertf
    :: TH1F -> CUInt -> IO Float
