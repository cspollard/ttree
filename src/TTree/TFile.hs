{-# LANGUAGE ForeignFunctionInterface #-}

module TTree.TFile
  ( TFile(..)
  , tfileOpen, tfileClose, tfileGet
  ) where


import           Foreign                hiding (void)
import           Foreign.C.String


newtype TFile = TFile (Ptr ())

foreign import ccall "tfileC.h tfileOpen" _tfileOpen
    :: CString -> IO TFile
foreign import ccall "tfileC.h tfileClose" _tfileClose
    :: TFile -> IO ()
foreign import ccall "tfileC.h tfileGet" _tfileGet
    :: TFile -> CString -> IO (Ptr ())


tfileOpen :: String -> IO TFile
tfileOpen = flip withCString _tfileOpen

tfileGet :: TFile -> String -> IO (Ptr ())
tfileGet tf s = withCString s $ _tfileGet tf

tfileClose :: TFile -> IO ()
tfileClose = _tfileClose
