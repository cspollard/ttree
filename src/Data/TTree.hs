{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.TTree where

import Control.Lens
import Conduit

import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Control.Monad ((>=>), (<=<), when)
import Control.Monad.State
import Data.Foldable (foldrM)


-- void pointer
type VPtr = Ptr ()

-- pointer to a c++ vector
newtype VecPtr a = VecPtr { _vptr :: VPtr } deriving (Show, Storable)

foreign import ccall "ttreeC.h tchain" _tchain
    :: CString -> IO VPtr
foreign import ccall "ttreeC.h tchainAdd" _tchainAdd
    :: VPtr -> CString -> IO Int
foreign import ccall "ttreeC.h tchainGetEntry" _tchainGetEntry
    :: VPtr -> Int -> IO Int
foreign import ccall "ttreeC.h tchainSetBranchAddress" _tchainSetBranchAddress
    :: VPtr -> CString -> Ptr a -> IO ()
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


-- I think there is probably a better way to do all of this, but since
-- I need to know the type of each branch at *run time*, the many
-- constructors to TBranch seem necessary.

-- TTreeValue, which seems like a "double" of TBranch could hopefully
-- be done away with some how, but for now I need it to be the pure
-- equivalent of TBranch--and what people use in practice.


{-
newtype STLVec a = STLVec { vecPtr :: ForeignPtr (Ptr ()) }
                          deriving (Eq, Show)

class Storable a => Vecable a where
    sizeV :: STLVec a -> Int
    dataV :: STLVec a -> Ptr a

instance Vecable Char where
    sizeV = withForeignPtr . vecPtr . vectorSizeC
    dataV = withForeignPtr . vecPtr . vectorDataC

instance Vecable Int where
    sizeV = withForeignPtr . vecPtr . vectorSizeI
    dataV = withForeignPtr . vecPtr . vectorDataI

instance Vecable Float where
    sizeV = withForeignPtr . vecPtr . vectorSizeF
    dataV = withForeignPtr . vecPtr . vectorDataF

instance Vecable Double where
    sizeV = withForeignPtr . vecPtr . vectorSizeD
    dataV = withForeignPtr . vecPtr . vectorDataD


peekV :: Vecable a => VecPtr a -> IO [a]
peekV v = peekArray (sizeV v) (dataV v)
-}




-- TODO
-- vector<vector<int> > and similar won't work.
            -- scalar branches

type TChain = ForeignPtr ()


tchain :: String -> IO TChain
tchain n = withCString n $ newForeignPtr _tchainFree <=< _tchain


addFile :: TChain -> String -> IO ()
addFile cp fn = withCString fn $ \s -> void (withForeignPtr cp (`_tchainAdd` s))


type ChainRead m a = StateT (TChain, Int, HashMap String VPtr) m a

getEntry :: MonadIO m => a -> ChainRead m (Maybe a)
getEntry x = do (cp, i, _) <- get
                n <- liftIO $ withForeignPtr cp $ flip _tchainGetEntry i
                return $ if n > 0 then Just x else Nothing


readBranch :: (MonadIO m, Branchable a, Storable (PtrType a)) => String -> StateT (TChain, Int, HashMap String VPtr) m a
readBranch s = do (cp, i, hm) <- get

                  case hm ^? ix s of
                       Just p  -> liftIO . fromBranch . castPtr $ p
                       Nothing -> do bp <- liftIO calloc
                                     put (cp, i, at s ?~ castPtr bp $ hm)
                                     liftIO $ withCString s $ \n -> withForeignPtr cp $ \p -> _tchainSetBranchAddress p n bp
                                     liftIO $ fromBranch bp



class Branchable b where
    type PtrType b :: *
    fromBranch :: Ptr (PtrType b) -> IO b

instance Branchable Float where
    type PtrType Float = Float
    fromBranch = peek

instance Branchable CLong where
    type PtrType CLong = CLong
    fromBranch = peek


class FromChain fc where
    fromChain :: MonadIO m => ChainRead m fc

data Event = Event Float CLong deriving Show

instance FromChain Event where
    fromChain = Event <$> readBranch "mu" <*> readBranch "eventNumber"


runChain :: MonadIO m => ChainRead (ConduitM i o m) o -> TChain -> ConduitM i o m ()
runChain f c = loop c 0 mempty
    where loop c' i hm = do (ms, (_, _, hm')) <- runStateT (f >>= getEntry) (c', i, hm)
                            case ms of
                                 Just x  -> yield x >> loop c' (i+1) hm'
                                 Nothing -> return ()


{-

runChainN :: MonadIO m => Int -> TChain -> Producer m TTree
runChainN n c = runChain c =$= takeC n


-- this class should be the main way in which people interact with
-- TTrees.
class FromTTree ft where
    fromBranches :: Hashmap String TBranch -> Maybe ft
    neededBranches :: HashMap String (Storable a => ForeignPtr a -> TBranch )


-}
