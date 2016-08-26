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

import Control.Monad ((>=>))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe


-- void pointer
type VPtr = Ptr ()

foreign import ccall "ttreeC.h ttree" _ttree
    :: CString -> CString -> IO VPtr
foreign import ccall "ttreeC.h ttreeGetEntry" _ttreeGetEntry
    :: VPtr -> Int -> IO Int
foreign import ccall "ttreeC.h ttreeGetBranchEntry" _ttreeGetBranchEntry
    :: VPtr -> CString -> Int -> Ptr a -> IO Int
foreign import ccall "ttreeC.h &ttreeFree" _ttreeFree
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


type TTree = ForeignPtr ()


ttree :: String -> String -> IO TTree
ttree tn fn = do tn' <- newCString tn
                 fn' <- newCString fn
                 tp <- newForeignPtr _ttreeFree =<< _ttree tn' fn'
                 free tn' >> free fn'
                 return tp


type TTreeRead m a = ReaderT (TTree, Int) (MaybeT m) a

-- getEntry :: MonadIO m => a -> TTreeRead m (Maybe a)
-- getEntry x = do (cp, i, _) <- get
                -- n <- liftIO $ withForeignPtr cp $ flip _ttreeGetEntry i
                -- return $ if n > 0 then Just x else Nothing


readBranch :: (MonadIO m, Branchable a, Storable (PtrType a)) => String -> TTreeRead m a
readBranch s = do (cp, i) <- ask
                  bp <- liftIO calloc
                  n <- liftIO $ withCString s $ \s' -> withForeignPtr cp $ \cp' -> _ttreeGetBranchEntry cp' s' i bp
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


class FromTTree fc where
    fromTTree :: MonadIO m => TTreeRead m fc


runTTree :: Monad m => TTreeRead (ConduitM i o m) o -> TTree -> ConduitM i o m ()
runTTree f c = loop c 0 
    where loop c' i = do ms <- runMaybeT $ runReaderT f (c', i)
                         case ms of
                              Just x  -> yield x >> loop c' (i+1)
                              Nothing -> return ()


runTTreeN :: Monad m => Int -> TTreeRead (ConduitM i o m) o -> TTree -> ConduitM i o m ()
runTTreeN n f c = runTTree f c =$= takeC n


project :: (MonadIO m, FromTTree fc) => TTree -> ConduitM i fc m ()
project = runTTree fromTTree
