module Main where

import           Control.Monad      (forM_)
import           Data.TFile
import           Data.TTree
import           Data.Vector        (Vector)
import           Pipes
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
      let ex =
            runEffect
            $ for (runTTree (fromTTree :: TreeRead IO Event) t) (liftIO . print)

          deal EndOfTTree         = return ()
          deal (TBranchReadFailure s) = print s

      catch ex deal
      tfileClose f
