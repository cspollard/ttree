module Main where

import Data.TTree
import Conduit
import System.Environment (getArgs)
import Foreign.C.Types (CLong)

data Event = Event Float CLong [Float] [Float] [Float] deriving Show

instance FromChain Event where
    fromChain = Event <$> readBranch "mu"
                      <*> readBranch "eventNumber"
                      <*> readBranch "jet_pt"
                      <*> readBranch "jet_eta"
                      <*> readBranch "jet_phi"

main :: IO ()
main = do (cn:fns) <- getArgs
          c <-  tchain cn
          mapM_ (addFile c) fns

          project c $$ mapM_C (print :: Event -> IO ())
