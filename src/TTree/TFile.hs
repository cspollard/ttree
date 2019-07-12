{-# LANGUAGE ForeignFunctionInterface #-}

module TTree.TFile
  ( module X
  , TFile(..)
  , tfileOpen, tfileClose, tfileGet
  ) where


import           Control.Monad.IO.Class as X (MonadIO (..))
import           Foreign                hiding (void)
import           Foreign.C.String


newtype TFile = TFile (Ptr ())

foreign import ccall "tfileC.h tfileOpen" _tfileOpen
    :: CString -> IO TFile
foreign import ccall "tfileC.h tfileClose" _tfileClose
    :: TFile -> IO ()
foreign import ccall "tfileC.h tfileGet" _tfileGet
    :: TFile -> CString -> IO (Ptr ())


tfileOpen :: MonadIO m => String -> m TFile
tfileOpen = liftIO . flip withCString _tfileOpen

tfileGet :: MonadIO m => TFile -> String -> m (Ptr ())
tfileGet tf s = liftIO . withCString s $ _tfileGet tf

tfileClose :: MonadIO m => TFile -> m ()
tfileClose = liftIO . _tfileClose
