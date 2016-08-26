module Main where

import Conduit
import System.Environment (getArgs)
import Foreign.C.Types (CLong)
import Control.Monad (forM_)
import Data.TTree

data Event = Event Float CLong [Float] [Float] [Float] deriving Show

instance FromTTree Event where
    fromTTree = Event <$> readBranch "mu"
                      <*> readBranch "eventNumber"
                      <*> readBranch "jet_pt"
                      <*> readBranch "jet_eta"
                      <*> readBranch "jet_phi"

main :: IO ()
main = do (tn:fns) <- getArgs
          ts <- mapM (ttree tn) fns

          forM_ ts $ \t -> print =<< (project t =$= mapMC (print :: Event -> IO ()) $$ lengthC)
