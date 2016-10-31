{-# LANGUAGE ForeignFunctionInterface #-}

module Data.TH1 where

import Foreign hiding (void)

import Foreign.C.Types (CUInt(..))
import Foreign.C.String

type TH1D = Ptr ()
type TH1F = Ptr ()

foreign import ccall "th1C.h th1D" th1d
    :: CString -> CString -> IO TH1D
foreign import ccall "th1C.h nbinsD" nbinsd
    :: TH1D -> IO CUInt
foreign import ccall "th1C.h binedgeD" binedged
    :: TH1D -> CUInt -> IO Double
foreign import ccall "th1C.h entryD" entryd
    :: TH1D -> CUInt -> IO Double
foreign import ccall "th1C.h uncertD" uncertd
    :: TH1D -> CUInt -> IO Double

foreign import ccall "th1C.h th1F" th1f
    :: CString -> CString -> IO TH1F
foreign import ccall "th1C.h nbinsF" nbinsf
    :: TH1F -> IO CUInt
foreign import ccall "th1C.h binedgeF" binedgef
    :: TH1F -> CUInt -> IO Double
foreign import ccall "th1C.h entryF" entryf
    :: TH1F -> CUInt -> IO Double
foreign import ccall "th1C.h uncertF" uncertf
    :: TH1F -> CUInt -> IO Double
