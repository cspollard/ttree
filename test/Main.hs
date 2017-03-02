module Main where

import           Control.Monad      (forM)
import           Data.Vector        (Vector)
import           List.Transformer
import           System.Environment (getArgs)

import           Data.TFile
import           Data.TTree

data Event = Event CInt CInt Float (Vector Float) (Vector Float) (Vector Float) (VVector Double) (VVector CInt) deriving Show

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
  ns <- forM fns
    $ \fn -> do
      f <- tfileOpen fn
      t <- ttree f tn
      n <- foldM perEvent (return 0) return . project $ t
      tfileClose f
      return n

  print $ sum ns

  where
    perEvent :: MonadIO m => Int -> Event -> m Int
    perEvent i e = do
      liftIO $ print e
      return $ i+1
