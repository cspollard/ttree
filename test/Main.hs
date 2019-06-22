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


test :: Final Applicative (BVar String) Float
test = do
  y <- inject $ BS @Float "mu"
  x <- inject $ BS @Float "mu"
  return $ x + y * 2



main :: IO ()
main = do
  (fname : tname : _) <- getArgs

  f <- tfileOpen fname

  t@(TTree tp) <- ttree f tname

  print =<< loadEntry t 0

  branches <-
    traverse getFirst <<< getI unBVar
    $ hmap (mapBVar (uncurry HM.singleton) <<< withPtr) test

  mapM_ print $ HM.keys branches

  binterpret id (either error return) $ connectBranches tp branches

  b <- interpret (runWithPtrs branches >>> mapBVar (either error id) >>> readBVar) test

  print b


-- data Event =
--   Event
--     CInt
--     CInt
--     Float
--     (Vector Float)
--     (Vector Float)
--     (Vector Float)
--     (VVector Double)
--     (VVector CInt)
--     deriving Show
-- 
-- instance FromTTree Event where
--   fromTTree =
--     Event
--       <$> readBranch "Run"
--       <*> readBranch "Event"
--       <*> readBranch "Mu"
--       <*> readBranch "JetPt"
--       <*> readBranch "JetEta"
--       <*> readBranch "JetPhi"
--       <*> readBranch "JetTracksPt"
--       <*> readBranch "JetTracksisTight"
-- 
-- main :: IO ()
-- main = do
--   (tn:fns) <- getArgs
--   forM_ fns
--     $ \fn -> do
--       f <- tfileOpen fn
--       t <- ttree f tn
--       runEffect . evalStateP t
--         $ each [0..]
--           >-> pipeTTree (fromTTree :: TreeRead IO Event)
--           >-> P.print
-- 
--       tfileClose f
