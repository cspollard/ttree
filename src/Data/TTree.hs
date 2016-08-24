{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.TTree where


import Foreign
import Foreign.C.String

import Control.Lens
import Conduit
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM

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


            -- scalar branches
data TBranch = TBChar (ForeignPtr Char)
             | TBInt (ForeignPtr Int)
             | TBFloat (ForeignPtr Float)
             | TBDouble (ForeignPtr Double)
            -- vector branches
             | TBVChar (ForeignPtr (VecPtr Char))
             | TBVInt (ForeignPtr (VecPtr Int))
             | TBVFloat (ForeignPtr (VecPtr Float))
             | TBVDouble (ForeignPtr (VecPtr Double))
             deriving Show

makePrisms ''TBranch



readS :: Storable a => Prism' TBranch (ForeignPtr a) -> TBranch -> IO (Maybe a)
readS p b = traverse (`withForeignPtr` peek) $ b ^? p

readV :: Vectorizable a => Prism' TBranch (ForeignPtr (VecPtr a)) -> TBranch -> IO (Maybe [a])
readV p b = traverse (`withForeignPtr` (peek >=> peekV)) $ b ^? p



readC :: TBranch -> IO (Maybe Char)
readC = readS _TBChar

readVC :: TBranch -> IO (Maybe String)
readVC = readV _TBVChar

readI :: TBranch -> IO (Maybe Int)
readI = readS _TBInt

readVI :: TBranch -> IO (Maybe [Int])
readVI = readV _TBVInt

readF :: TBranch -> IO (Maybe Float)
readF = readS _TBFloat

readVF :: TBranch -> IO (Maybe [Float])
readVF = readV _TBVFloat

readD :: TBranch -> IO (Maybe Double)
readD = readS _TBDouble

readVD :: TBranch -> IO (Maybe [Double])
readVD = readV _TBVDouble


data TChain = TChain { _cPtr :: VPtr
                     , _cBranches :: HashMap String TBranch
                     } deriving Show

makeLenses ''TChain

tchain :: String -> IO TChain
tchain n = do s <- newCString n
              c <- _tchain s
              free s
              return $ TChain c HM.empty


addFile :: TChain -> String -> IO TChain
addFile tc@(TChain cp _) fn = do s <- newCString fn
                                 _tchainAdd cp s
                                 free s
                                 return tc


addBranchS :: Storable a => (ForeignPtr a -> TBranch) -> TChain -> String -> IO TChain
addBranchS f (TChain cp cbs) n = do p <- mallocForeignPtr
                                    s <- newCString n
                                    withForeignPtr p $ _tchainSetBranchAddress cp s
                                    free s
                                    return $ TChain cp (cbs & at n ?~ f p)


addBranchV :: Vectorizable a => (ForeignPtr (VecPtr a) -> TBranch) -> TChain -> String -> IO TChain
addBranchV f (TChain cp cbs) n = do p <- mallocForeignPtr
                                    s <- newCString n
                                    withForeignPtr p $ _tchainSetBranchAddress cp s
                                    free s
                                    return $ TChain cp (cbs & at n ?~ f p)

addBranchF = addBranchS TBFloat
addBranchVF = addBranchV TBVFloat


getEntry :: TChain -> Int -> IO (Maybe TChain)
getEntry tc@(TChain cp _) i = do n <- _tchainGetEntry cp i
                                 return $ if n < 0
                                             then Nothing
                                             else Just tc


printBranch :: TBranch -> IO ()
printBranch (TBFloat p)  = print =<< withForeignPtr p peek
printBranch (TBVFloat p) = do p' <- withForeignPtr p peek
                              print =<< peekArray (vectorSizeF p') (vectorDataF p')
