{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.TTree ( ttree
                  , Branchable(..), readBranch
                  , TTreeRead, FromTTree(..)
                  , runTTree, runTTreeN, project
                  , MonadIO(..)
                  ) where

import Conduit

import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String

import Control.Monad ((>=>))
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

import Data.Vector.Storable hiding ((++))
import qualified Data.Vector as V
import Control.Applicative (ZipList(..))


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



-- getEntry :: MonadIO m => a -> TTreeRead m (Maybe a)
-- getEntry x = do (cp, i, _) <- get
                -- n <- liftIO $ withForeignPtr cp $ flip _ttreeGetEntry i
                -- return $ if n > 0 then Just x else Nothing


class Branchable b where
    type HeapType b :: *
    fromBranch :: Ptr (HeapType b) -> IO b

instance Branchable Char where
    type HeapType Char = Char
    fromBranch = peek

instance Branchable Int where
    type HeapType Int = Int
    fromBranch = peek

instance Branchable CUInt where
    type HeapType CUInt = CUInt
    fromBranch = peek

instance Branchable CLong where
    type HeapType CLong = CLong
    fromBranch = peek

instance Branchable Float where
    type HeapType Float = Float
    fromBranch = peek

instance Branchable Double where
    type HeapType Double = Double
    fromBranch = peek


-- pointer to a c++ vector
newtype VecPtr a = VecPtr VPtr deriving (Show, Storable)


class Storable a => Vecable a where
    sizeV :: VecPtr a -> Int
    dataV :: VecPtr a -> Ptr a
    toV :: VecPtr a -> IO (Vector a)
    toV vp = flip unsafeFromForeignPtr0 (sizeV vp) <$> newForeignPtr_ (dataV vp)

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


instance Vecable a => Branchable (V.Vector a) where
    type HeapType (V.Vector a) = VecPtr a
    fromBranch = peek >=> toV >=> return . convert

instance Vecable a => Branchable [a] where
    type HeapType [a] = VecPtr a
    fromBranch = peek >=> toV >=> return . toList

instance Vecable a => Branchable (ZipList a) where
    type HeapType (ZipList a) = VecPtr a
    fromBranch = peek >=> toV >=> return . ZipList . toList


type TTreeRead m a = ReaderT (TTree, Int) (MaybeT m) a

readBranch :: (MonadIO m, Branchable a, Storable (HeapType a)) => String -> TTreeRead m a
readBranch s = do (cp, i) <- ask
                  bp <- liftIO calloc
                  n <- liftIO $ withCString s $ \s' -> withForeignPtr cp $ \cp' -> _ttreeGetBranchEntry cp' s' i bp
                  if n <= 0
                     then liftIO (free bp) >> fail ("failed to read branch " ++ s)
                     else liftIO $ fromBranch bp <* free bp


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
