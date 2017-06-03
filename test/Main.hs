module Main where

import           Control.Monad      (forM_)
import           Data.TFile
import           Data.TTree
import           Data.Vector        (Vector)
import           Pipes
import           Pipes.Lift
import qualified Pipes.Prelude      as P
import           System.Environment (getArgs)

data Event =
  Event
    CInt
    CInt
    Float
    (Vector Float)
    (Vector Float)
    (Vector Float)
    (VVector Double)
    (VVector CInt)
    deriving Show

instance FromTTree Event where
  fromTTree =
    Event
      <$> readBranch "Run"
      <*> readBranch "Event"
      <*> readBranch "Mu"
      <*> readBranch "JetPt"
      <*> readBranch "JetEta"
      <*> readBranch "JetPhi"
      <*> readBranch "JetTracksPt"
      <*> readBranch "JetTracksisTight"

main :: IO ()
main = do
  (tn:fns) <- getArgs
  forM_ fns
    $ \fn -> do
      f <- tfileOpen fn
      t <- ttree f tn
      runEffect . evalStateP t
        $ each [0..]
          >-> pipeTTree (fromTTree :: TreeRead IO Event)
          >-> P.print

      tfileClose f
