module Main where

import List.Transformer

import System.Environment (getArgs)
import Control.Monad (forM)
import Data.Vector (Vector)

import Data.TTree

data Event = Event CInt CInt Float (Vector Float) (Vector Float) (Vector Float) (VVector Double) (VVector CInt) deriving Show

instance FromTTree Event where
    fromTTree = Event <$> readBranch "Run"
                      <*> readBranch "Event"
                      <*> readBranch "Mu"
                      <*> readBranch "JetPt"
                      <*> readBranch "JetEta"
                      <*> readBranch "JetPhi"
                      <*> readBranch "JetTracksPt"
                      <*> readBranch "JetTracksisTight"

main :: IO ()
main = do (tn:fns) <- getArgs
          ts <- mapM (ttree tn) fns

          ns <- forM ts $ foldM f (return 0) return . project

          print $ sum ns

    where
        f :: Int -> Event -> IO Int
        f i e = do
            print e
            return $ i+1
