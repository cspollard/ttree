module Main where

import Conduit
import System.Environment (getArgs)
import Control.Monad (forM)
import Data.Vector (Vector)

import Data.TBranch
import Data.TTree

data Event = Event CInt CInt Float (Vector Float) (Vector Float) (Vector Float) (VVector Double) deriving Show

instance FromTTree Event where
    fromTTree = Event <$> readBranch "Run"
                      <*> readBranch "Event"
                      <*> readBranch "Mu"
                      <*> readBranch "JetPt"
                      <*> readBranch "JetEta"
                      <*> readBranch "JetPhi"
                      <*> readBranch "JetTracksPt"

main :: IO ()
main = do (tn:fns) <- getArgs
          ts <- mapM (ttree tn) fns

          ns <- forM ts $ \t -> project t =$= mapMC (print :: Event -> IO ()) $$ lengthC
          print (sum ns :: Int)
