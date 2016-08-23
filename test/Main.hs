module Main where

import Data.TTree
import Foreign
import Foreign.C

main :: IO ()
main = do tname <- newCString "nominal"
          fname <- newCString "data/test.root"
          wname <- newCString "mu"
          c <- tchain tname
          tchainAdd c fname

          ptr <- calloc :: IO (Ptr CFloat)
          print ptr
          print =<< peek ptr
          tchainSetBranchAddress c wname ptr
          tchainGetEntry c 0
          print ptr
          print =<< peek ptr
          free tname >> free fname >> free wname >> free ptr
