{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Main where


import System.Environment (getArgs)
import Data.BVar
import Data.TTree
import Data.Monoid (Product(..))
import Data.TFile
import Data.Functor.Combinator
import Data.HFunctor
import Control.Arrow ((>>>), (<<<))
import qualified Data.Vector as V


data Tell s a where
  Tell :: s -> Tell s ()



test :: Free (Tell (Product Double) :+: BVar String) (Float, Float, Float)
test = do
  mu <- s "mu"
  y <- v "jet_pt"
  if y V.! 1 > 20
    then sf $ Product 0.9
    else sf $ Product 1.2
  x <- v "jet_eta"
  z <- v2 "jet_pv_track_pt"
  sf $ Product 1.1
  return $ (mu, V.head x + V.head y * 2, (V.head >>> V.head) z)

  where
    s = inject <<< inR <<< BS
    v = inject <<< inR <<< BV
    v2 = inject <<< inR <<< BVV
    sf = inject <<< inL <<< Tell


collapseTell :: Free (Tell s :+: f) a -> Free f a
collapseTell = hbind $ \case
  L1 (Tell _) -> pure ()
  R1 b -> inject b


interpTell :: Tell s ~> (,) s
interpTell (Tell s) = (s, ())


type Writer s = Comp IO ((,) s)


interpWriter
  :: Monoid sf
  => HM String VFP -> (Tell sf :+: BVar String) ~> Writer sf
interpWriter _ (L1 (Tell sf)) = Comp $ return (sf, ())
interpWriter branches (R1 b) = Comp <<< fmap (mempty,) $ readVars branches b


main :: IO ()
main = do
  (fname : tname : _) <- getArgs

  f <- tfileOpen fname

  t@(TTree tp) <- ttree f tname

  branches <- marshalling (collapseTell test)

  binterpret id (either error return) $ connectBranches tp branches

  let prog = unComp $ interpret (interpWriter branches) test

  print =<< loadEntry t 0

  print =<< prog

  print =<< loadEntry t 1

  print =<< prog

  print =<< loadEntry t 100

  print =<< prog
