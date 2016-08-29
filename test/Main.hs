module Main where

import Conduit
import System.Environment (getArgs)
import Foreign.C.Types (CLong)
import Control.Monad (forM)
import Data.TTree
import Data.Vector (Vector)

data Event = Event Int Int Float (Vector Float) (Vector Float) (Vector Float) deriving Show

instance FromTTree Event where
    fromTTree = Event <$> readBranch "Run"
                      <*> readBranch "Event"
                      <*> readBranch "Mu"
                      <*> readBranch "JetPt"
                      <*> readBranch "JetEta"
                      <*> readBranch "JetPhi"

main :: IO ()
main = do (tn:fns) <- getArgs
          ts <- mapM (ttree tn) fns

          ns <- forM ts $ \t -> project t =$= mapMC (print :: Event -> IO ()) $$ lengthC
          print (sum ns :: Int)
