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
import Control.Monad.Trans.Except


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


type ChainRead m a = ReaderT (TChain, Int) (ExceptT String m) a

-- getEntry :: MonadIO m => a -> ChainRead m (Maybe a)
-- getEntry x = do (cp, i, _) <- get
                -- n <- liftIO $ withForeignPtr cp $ flip _tchainGetEntry i
                -- return $ if n > 0 then Just x else Nothing


readBranch :: (MonadIO m, Branchable a, Storable (PtrType a)) => String -> ChainRead m a
readBranch s = do (cp, i) <- ask
                  bp <- liftIO calloc
                  n <- liftIO $ withCString s $ \s' -> withForeignPtr cp $ \cp' -> _tchainGetBranchEntry cp' s' i bp
                  if n <= 0
                     then fail $ "failed to read branch " ++ s
                     else liftIO $ fromBranch bp <* free bp



class Branchable b where
    type PtrType b :: *
    fromBranch :: Ptr (PtrType b) -> IO b

instance Branchable Char where
    type PtrType Char = Char
    fromBranch = peek

instance Branchable Int where
    type PtrType Int = Int
    fromBranch = peek

instance Branchable CUInt where
    type PtrType CUInt = CUInt
    fromBranch = peek

instance Branchable CLong where
    type PtrType CLong = CLong
    fromBranch = peek

instance Branchable Float where
    type PtrType Float = Float
    fromBranch = peek

instance Branchable Double where
    type PtrType Double = Double
    fromBranch = peek


-- pointer to a c++ vector
newtype VecPtr a = VecPtr VPtr deriving (Show, Storable)


class Vecable a where
    sizeV :: VecPtr a -> Int
    dataV :: VecPtr a -> Ptr a
    peekV :: VecPtr a -> IO [a]


instance Vecable Char where
    sizeV = vectorSizeC
    dataV = vectorDataC
    peekV v = peekArray (sizeV v) (dataV v)

instance Vecable Int where
    sizeV = vectorSizeI
    dataV = vectorDataI
    peekV v = peekArray (sizeV v) (dataV v)

instance Vecable Float where
    sizeV = vectorSizeF
    dataV = vectorDataF
    peekV v = peekArray (sizeV v) (dataV v)

instance Vecable Double where
    sizeV = vectorSizeD
    dataV = vectorDataD
    peekV v = peekArray (sizeV v) (dataV v)



instance Vecable a => Branchable [a] where
    type PtrType [a] = VecPtr a
    fromBranch = peek >=> peekV


class FromChain fc where
    fromChain :: MonadIO m => ChainRead m fc


runChain :: Monad m => ChainRead (ConduitM i o m) o -> TChain -> ConduitM i o m ()
runChain f c = loop c 0 
    where loop c' i = do ms <- runExceptT $ runReaderT f (c', i)
                         case ms of
                              Right x  -> yield x >> loop c' (i+1)
                              Left err -> fail err


runChainN :: Monad m => Int -> ChainRead (ConduitM i o m) o -> TChain -> ConduitM i o m ()
runChainN n f c = runChain f c =$= takeC n


project :: (MonadIO m, FromChain fc) => TChain -> Producer m fc
project = runChain fromChain
