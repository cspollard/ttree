module Main where

import Data.TTree
import Foreign.Ptr
import Foreign.C.String

main :: IO ()
main = do s <- newCString "asdf"
          ttreeSetBranchAddress (nullPtr :: Ptr ()) s (nullPtr :: Ptr (Ptr ()))
