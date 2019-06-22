{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}

module Main where


import System.Environment (getArgs)
import Foreign.C.Types
import qualified Data.HashMap.Monoidal as HM
import Data.Semigroup (First(..))
import Data.TBranch
import Data.TTree
import Data.TFile
import Data.Functor.Combinator
import Control.Arrow ((<<<), (>>>))
import qualified Data.Vector as V


test :: Final Applicative (BVar String) (Float, Float)
test = do
  y <- inject $ BV @Float "jet_pt"
  x <- inject $ BV @Float "jet_eta"
  z <- inject $ BVV @Float "jet_pv_track_pt"
  return $ (V.head x + V.head y * 2, (V.head >>> V.head) z)



main :: IO ()
main = do
  (fname : tname : _) <- getArgs

  f <- tfileOpen fname

  t@(TTree tp) <- ttree f tname

  print =<< loadEntry t 0

  branches <-
    traverse getFirst <<< getI unBVar
    $ hmap (mapBVar (uncurry HM.singleton) <<< withPtr) test

  mapM_ putStrLn $ HM.keys branches

  binterpret id (either error return) $ connectBranches tp branches

  b <- interpret (runWithPtrs branches >>> mapBVar (either error id) >>> readBVar) test

  print b
