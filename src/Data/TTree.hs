{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.TTree where

import Conduit

import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String

import Control.Monad ((>=>), (<=<), void)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe


-- void pointer
type VPtr = Ptr ()

foreign import ccall "ttreeC.h tchain" _tchain
    :: CString -> IO VPtr
foreign import ccall "ttreeC.h tchainAdd" _tchainAdd
    :: VPtr -> CString -> IO Int
foreign import ccall "ttreeC.h tchainGetEntry" _tchainGetEntry
    :: VPtr -> Int -> IO Int
foreign import ccall "ttreeC.h tchainGetBranchEntry" _tchainGetBranchEntry
    :: VPtr -> CString -> Int -> Ptr a -> IO Int
foreign import ccall "ttreeC.h &tchainFree" _tchainFree
    :: FunPtr (Ptr a -> IO ())

foreign import ccall "ttreeC.h vectorSizeC" vectorSizeC
    :: VecPtr Char -> Int
foreign import ccall "ttreeC.h vectorDataC" vectorDataC
    :: VecPtr Char -> Ptr Char
foreign import ccall "ttreeC.h vectorSizeI" vectorSizeI
    :: VecPtr Int -> Int
foreign import ccall "ttreeC.h vectorDataI" vectorDataI
    :: VecPtr Int -> Ptr Int
foreign import ccall "ttreeC.h vectorSizeF" vectorSizeF
    :: VecPtr Float -> Int
foreign import ccall "ttreeC.h vectorDataF" vectorDataF
    :: VecPtr Float -> Ptr Float
foreign import ccall "ttreeC.h vectorSizeD" vectorSizeD
    :: VecPtr Double -> Int
foreign import ccall "ttreeC.h vectorDataD" vectorDataD
    :: VecPtr Double -> Ptr Double




-- TODO
-- vector<vector<int> > and similar won't work.
            -- scalar branches

type TChain = ForeignPtr ()


tchain :: String -> IO TChain
tchain n = withCString n $ newForeignPtr _tchainFree <=< _tchain


addFile :: TChain -> String -> IO ()
addFile cp fn = withCString fn $ \s -> void (withForeignPtr cp (`_tchainAdd` s))


type ChainRead m a = ReaderT (TChain, Int) (MaybeT m) a

-- getEntry :: MonadIO m => a -> ChainRead m (Maybe a)
-- getEntry x = do (cp, i, _) <- get
                -- n <- liftIO $ withForeignPtr cp $ flip _tchainGetEntry i
                -- return $ if n > 0 then Just x else Nothing


readBranch :: (MonadIO m, Branchable a, Storable (PtrType a)) => String -> ChainRead m a
readBranch s = do (cp, i) <- ask
                  liftIO . alloca $ go s cp i

    where go s' cp' i' bp' = do tmp <- calloc         -- TODO
                                poke bp' =<< peek tmp -- this looks silly, but I need the bytes to contain only zeros...
                                free tmp
                                n <- withCString s' $ \n -> withForeignPtr cp' $ \p -> _tchainGetBranchEntry p n i' bp'
                                if n > 0 then fromBranch bp' else fail $ "failed to read branch " ++ s'



class Branchable b where
    type PtrType b :: *
    fromBranch :: Ptr (PtrType b) -> IO b

instance Branchable Float where
    type PtrType Float = Float
    fromBranch = peek

instance Branchable CLong where
    type PtrType CLong = CLong
    fromBranch = peek


-- pointer to a c++ vector
newtype VecPtr a = VecPtr { _vptr :: VPtr } deriving (Show, Storable)

peekV :: Vecable a => VecPtr a -> IO [a]
peekV v = peekArray (sizeV v) (dataV v)


class Storable a => Vecable a where
    sizeV :: VecPtr a -> Int
    dataV :: VecPtr a -> Ptr a


instance Vecable Char where
    sizeV = vectorSizeC
    dataV = vectorDataC

instance Vecable Int where
    sizeV = vectorSizeI
    dataV = vectorDataI

instance Vecable Float where
    sizeV = vectorSizeF
    dataV = vectorDataF

instance Vecable Double where
    sizeV = vectorSizeD
    dataV = vectorDataD


instance Vecable a => Branchable [a] where
    type PtrType [a] = VecPtr a
    fromBranch = peek >=> peekV


class FromChain fc where
    fromChain :: MonadIO m => ChainRead m fc

data Event = Event Float CLong [Float] [Float] [Float] deriving Show

instance FromChain Event where
    fromChain = Event <$> readBranch "mu"
                      <*> readBranch "eventNumber"
                      <*> readBranch "jet_pt"
                      <*> readBranch "jet_eta"
                      <*> readBranch "jet_phi"


runChain :: Monad m => ChainRead (ConduitM i o m) o -> TChain -> ConduitM i o m ()
runChain f c = loop c 0 
    where loop c' i = do ms <- runMaybeT $ runReaderT f (c', i)
                         case ms of
                              Just x  -> yield x >> loop c' (i+1)
                              Nothing -> return ()


runChainN :: Monad m => Int -> ChainRead (ConduitM i o m) o -> TChain -> ConduitM i o m ()
runChainN n f c = runChain f c =$= takeC n
