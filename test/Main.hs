module Main where

import Data.TTree
import Conduit
import System.Environment (getArgs)

main :: IO ()
main = do (cn:fns) <- getArgs
          c <-  tchain cn
          mapM_ (addFile c) fns

          runChain fromChain c $$ mapM_C (print :: Event -> IO ())
