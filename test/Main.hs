module Main where

import           Control.Monad      (forM_)
import           Data.Vector        (Vector)
import           Pipes
import           System.Environment (getArgs)

import           Data.TFile
import           Data.TTree

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
      e <-
        runTTree t
        $ for (produceTTree (fromTTree :: TreeRead IO Event)) (liftIO . print)

      print e
      tfileClose f
