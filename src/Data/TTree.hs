{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.TTree where


import Foreign
import Foreign.C.Types
import Foreign.C.String

import Control.Lens
import Conduit
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Control.Monad ((>=>))


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


class Storable a => Vectorizable a where
    vSize :: VecPtr a -> Int
    vData :: VecPtr a -> Ptr a

instance Vectorizable Char where
    vSize = vectorSizeC
    vData = vectorDataC

instance Vectorizable Int where
    vSize = vectorSizeI
    vData = vectorDataI

instance Vectorizable Float where
    vSize = vectorSizeF
    vData = vectorDataF

instance Vectorizable Double where
    vSize = vectorSizeD
    vData = vectorDataD


peekV :: Vectorizable a => VecPtr a -> IO [a]
peekV v = peekArray (vSize v) (vData v)


-- TODO
-- vector<vector<int> > and similar won't work.

               -- scalar branches
data TTreeValue = TVChar Char
                | TVInt Int
                | TVUInt CUInt
                | TVLong CLong
                | TVFloat Float
                | TVDouble Double
               -- vector branches
                | TVVChar [Char]
                | TVVInt [Int]
                | TVVFloat [Float]
                | TVVDouble [Double]
                deriving Show

makePrisms ''TTreeValue

type TTree = HashMap String TTreeValue


            -- scalar branches
data TBranch = TBChar (ForeignPtr Char)
             | TBInt (ForeignPtr Int)
             | TBUInt (ForeignPtr CUInt)
             | TBLong (ForeignPtr CLong)
             | TBFloat (ForeignPtr Float)
             | TBDouble (ForeignPtr Double)
            -- vector branches
             | TBVChar (ForeignPtr (VecPtr Char))
             | TBVInt (ForeignPtr (VecPtr Int))
             | TBVFloat (ForeignPtr (VecPtr Float))
             | TBVDouble (ForeignPtr (VecPtr Double))
             deriving Show

makePrisms ''TBranch



readBranch :: TBranch -> IO TTreeValue
readBranch (TBChar fp) = TVChar <$> withForeignPtr fp peek
readBranch (TBInt fp) = TVInt <$> withForeignPtr fp peek
readBranch (TBUInt fp) = TVUInt <$> withForeignPtr fp peek
readBranch (TBLong fp) = TVLong <$> withForeignPtr fp peek
readBranch (TBFloat fp) = TVFloat <$> withForeignPtr fp peek
readBranch (TBDouble fp) = TVDouble <$> withForeignPtr fp peek
readBranch (TBVChar fp) = TVVChar <$> withForeignPtr fp (peek >=> peekV)
readBranch (TBVInt fp) = TVVInt <$> withForeignPtr fp (peek >=> peekV)
readBranch (TBVFloat fp) = TVVFloat <$> withForeignPtr fp (peek >=> peekV)
readBranch (TBVDouble fp) = TVVDouble <$> withForeignPtr fp (peek >=> peekV)


data TChain = TChain { _cPtr :: VPtr
                     , _cBranches :: HashMap String TBranch
                     } deriving Show

makeLenses ''TChain


tchain :: String -> IO TChain
tchain n = do s <- newCString n
              c <- _tchain s
              free s
              return $ TChain c HM.empty


addFile :: TChain -> String -> IO ()
addFile (TChain cp _) fn = do s <- newCString fn
                              _ <- _tchainAdd cp s
                              free s


addBranch :: Storable a
          => (ForeignPtr a -> TBranch) -> String -> TChain -> IO TChain
addBranch f n (TChain cp cbs) = do p <- mallocForeignPtr
                                   s <- newCString n
                                   _ <- withForeignPtr p $ _tchainSetBranchAddress cp s
                                   free s
                                   return $ TChain cp (cbs & at n ?~ f p)


addBranchC, addBranchVC, addBranchI, addBranchU, addBranchL, addBranchVI
    :: String -> TChain -> IO TChain
addBranchC = addBranch TBChar
addBranchVC = addBranch TBVChar
addBranchI = addBranch TBInt
addBranchU = addBranch TBUInt
addBranchL = addBranch TBLong
addBranchVI = addBranch TBVInt

addBranchF, addBranchVF, addBranchD, addBranchVD
    :: String -> TChain -> IO TChain
addBranchF = addBranch TBFloat
addBranchVF = addBranch TBVFloat
addBranchD = addBranch TBDouble
addBranchVD = addBranch TBVDouble


getEntry :: Int -> TChain -> IO (Maybe TTree)
getEntry i (TChain cp cbs) = do n <- _tchainGetEntry cp i
                                if n <= 0
                                   then return Nothing
                                   else Just <$> traverse (liftIO . readBranch) cbs


runChain :: MonadIO m => TChain -> Producer m TTree
runChain c = loop 0 c
    where loop i c' = do mc <- liftIO (getEntry i c')
                         case mc of
                              Nothing -> return ()
                              Just tt -> yield tt >> loop (i+1) c'


runChainN :: MonadIO m => Int -> TChain -> Producer m TTree
runChainN n c = runChain c =$= takeC n


projectChain :: MonadIO m => TChain -> (a -> TTree -> a) -> a -> m a
projectChain c f x = runChain c $$ foldlC f x
