module Main where

import Data.TTree
import Foreign.Ptr (castPtr)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (free)

main :: IO ()
main = do tname <- newCString "nominal"
          fname <- newCString "data/test.root"
          c <- tchain tname
          tchainAdd c fname
          ttreeGetEntry (castPtr c) 0
          free tname >> free fname
