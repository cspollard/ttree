{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.TTree where


import Foreign
import Foreign.C.String

import Control.Lens
import Conduit
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM


-- void pointer
type VPtr = Ptr ()

-- pointer to a c++ vector
type VecPtr a = VPtr

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



class Branchable b where
    type BranchPtr b
    mkPtr :: b

            -- scalar branches
data TBranch = TBChar (Ptr Char)
             | TBInt (Ptr Int)
             | TBFloat (Ptr Float)
             | TBDouble (Ptr Double)
            -- vector branches
             | TBVChar (Ptr (VecPtr Char))
             | TBVInt (Ptr (VecPtr Int))
             | TBVFloat (Ptr (VecPtr Float))
             | TBVDouble (Ptr (VecPtr Double))
             deriving Show

makePrisms ''TBranch


readF :: TBranch -> IO (Maybe Float)
readF b = traverse peek $ b ^? _TBFloat

readVF :: TBranch -> IO (Maybe [Float])
readVF b = case b ^? _TBVFloat of
                Nothing -> return Nothing
                Just pp -> do p <- peek pp
                              Just <$> peekArray (vectorSizeF p) (vectorDataF p)


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


addBranchF :: TChain -> String -> IO TChain
addBranchF (TChain cp cbs) n = do p <- calloc
                                  s <- newCString n
                                  _tchainSetBranchAddress cp s p
                                  free s
                                  return $ TChain cp (cbs & at n ?~ TBFloat p)


addBranchVF :: TChain -> String -> IO TChain
addBranchVF (TChain cp cbs) n = do p <- calloc
                                   s <- newCString n
                                   _tchainSetBranchAddress cp s p
                                   free s
                                   return $ TChain cp (cbs & at n ?~ TBVFloat p)


getEntry :: TChain -> Int -> IO (Maybe TChain)
getEntry tc@(TChain cp _) i = do n <- _tchainGetEntry cp i
                                 return $ if n < 0
                                             then Nothing
                                             else Just tc


printBranch :: TBranch -> IO ()
printBranch (TBFloat p)  = print =<< peek p
printBranch (TBVFloat p) = do p' <- peek p
                              print =<< peekArray (vectorSizeF p') (vectorDataF p')
