module Main where

import Data.TTree
import Foreign
import Foreign.C

type VPtr = Ptr ()

main :: IO ()
main = do tname <- newCString "nominal"
          fname <- newCString "data/test.root"
          muname <- newCString "mu"
          jptname <- newCString "jet_pt"
          c <- tchain tname
          tchainAdd c fname

          ptr <- calloc :: IO (Ptr CFloat)
          vptr <- calloc :: IO (Ptr VPtr)
          tchainSetBranchAddress c muname ptr
          tchainSetBranchAddress c jptname vptr
          tchainGetEntry c 0
          print =<< peek ptr
          print =<< peek vptr

          p <- peek vptr
          print =<< peekArray (vectorSizeF p) (vectorDataF p)

          -- TODO
          -- free mallocs
