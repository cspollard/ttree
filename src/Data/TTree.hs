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
import Control.Monad.Trans.Cont
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



data TTree = TTree { _tptr :: ForeignPtr ()
                   , _tbrs :: HashMap String (ForeignPtr ())
                   } deriving Show


ttree :: String -> String -> IO TTree
ttree tn fn = do tn' <- newCString tn
                 fn' <- newCString fn
                 tp <- newForeignPtr _ttreeFree =<< _ttree tn' fn'
                 free tn' >> free fn'
                 return $ TTree tp mempty



-- ContT r m a :: (a -> m r) -> m r
-- you give me a way to get from TTree to x;
-- I'll give you x.
type TreeRead m r = ContT r m TTree

-- you give me a way to get from VPtr to x;
-- I'll give you x.
type BranchRead m r = ContT r m VPtr


class FromTTree a where
    fromTTree :: MonadIO m => TreeRead m a


-- I don't yet know where the branch's pointer is
-- I will only get it after GetEntry
-- If you give me the function to access the pointer, then I will
-- interpret it with type a.
-- readBranch :: (MonadIO m, Branchable a, Storable (HeapType a)) => String -> BranchRead m a
-- readBranch s = do p <- liftIO $ _tchainGetBranchAddress s
                  -- fromBranch p

-- TODO!!!!
-- callback: give me a string and I'll give you the pointer.
readBranch :: MonadIO m => TTree -> String -> IO (ForeignPtr a)
readBranch t s = do p <- _tchainGetBranchAddress t s
                    if p == nullPtr
                       then newForeignPtr finalizerFree calloc
                       else newForeignPtr_ p

getEntry :: MonadIO m => TTree -> Int -> ContT a m TTree
getEntry tp i f = do bs <- needsBranches f
                     fillBranches tp i bs
                     fillF f


{-
                                n <- liftIO $ withCString s $ \s' -> withForeignPtr tp $ \tp' -> _ttreeGetBranchEntry tp' s' bp
                                if n <= 0
                                   then liftIO (free bp) >> fail ("failed to read branch " ++ s)
                                   else liftIO $ fromBranch bp <* free bp

-}

{-

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
-}
