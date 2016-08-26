module Main where

import Data.TTree
import Conduit
import System.Environment (getArgs)
import Foreign.C.Types (CLong)

data Event = Event Float CLong [Float] [Float] [Float] deriving Show

instance FromTTree Event where
    fromTTree = Event <$> readBranch "mu"
                      <*> readBranch "eventNumber"
                      <*> readBranch "jet_pt"
                      <*> readBranch "jet_eta"
                      <*> readBranch "jet_phi"

main :: IO ()
main = do (cn:fn:_) <- getArgs
          c <-  ttree cn fn

          project c $$ mapM_C (print :: Event -> IO ())
