{-# LANGUAGE ForeignFunctionInterface #-}

module Data.TFile where

import           Foreign          hiding (void)
import           Foreign.C.String

newtype TFile = TFile (Ptr ())

foreign import ccall "tfileC.h tfileOpen" _tfileOpen
    :: CString -> IO TFile
foreign import ccall "tfileC.h tfileClose" tfileClose
    :: TFile -> IO ()
foreign import ccall "tfileC.h tfileGet" _tfileGet
    :: TFile -> CString -> IO (Ptr ())


tfileOpen :: String -> IO TFile
tfileOpen = flip withCString _tfileOpen

tfileGet :: TFile -> String -> IO (Ptr ())
tfileGet = flip withCString . _tfileGet
