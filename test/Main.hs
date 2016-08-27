module Main where

import Conduit
import System.Environment (getArgs)
import Foreign.C.Types (CLong)
import Control.Monad (forM)
import Data.TTree
import Data.Vector (Vector)

data Event = Event Float CLong (Vector Float) (Vector Float) (Vector Float) deriving Show

instance FromTTree Event where
    fromTTree = Event <$> readBranch "mu"
                      <*> readBranch "eventNumber"
                      <*> readBranch "jet_pt"
                      <*> readBranch "jet_eta"
                      <*> readBranch "jet_phi"

main :: IO ()
main = do (tn:fns) <- getArgs
          ts <- mapM (ttree tn) fns

          ns <- forM ts $ \t -> project t =$= mapMC (print :: Event -> IO ()) $$ lengthC
          print (sum ns :: Int)
