{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}



module Main where


import Prelude hiding (id, (.))
import System.Environment (getArgs)
import TTree.Branch
import TTree.TTree
import TTree.TFile
import Control.Arrow
import Data.Vector (Vector)
import Analysis.Free
import qualified Data.HashMap.Monoidal as HM
import Analysis.Const
import Control.Category
import Data.Profunctor
import Data.Functor.Compose
import Data.Profunctor.Traversing
import Data.Semigroup (First(..))
import Control.Monad (join)
import Data.Monoid (Product(..))
import Foreign



type Vars = Kleisli []

type SFs = Kleisli ((,) (Product Float))

sf :: SFs Float ()
sf = Kleisli $ \w -> (Product w, ())

type Ret = (Float, Vector Float, Vector Float, Vector (Vector Float))

ana
  :: Members arrs '[ L String BS, L String BV, L String BV2, Vars , SFs ]
  => Analysis arrs () Ret
ana = proc store -> do
  jpts <- vector "jet_pt" -< store
  jetas <- vector "jet_eta" -< store
  jpvtrkpt <- vector2 "jet_pv_track_pt" -< store
  () <- liftFree <<< inj $ sf -< 1.1
  jpts' <- liftFree <<< inj $ Kleisli (\p -> [(*0.5) <$> p, p]) -< jpts
  mu <- scalar "mu" -< store
  
  returnA -< (mu, jpts, jetas, jpvtrkpt)


hoistFree' :: Free p a b -> (p :-> q) -> Free q a b
hoistFree' a f = hoistFree f a


runFree' :: (Category q, Traversing q) => Free p a b -> (p :-> q) -> q a b
runFree' a f = runFree f a

toConst :: Monoid m => p :-> Const m
toConst _ = Const mempty

ignVars :: Monoid m => Vars :-> Const m
ignVars = toConst

ignSFs :: Monoid m => SFs :-> Const m
ignSFs = toConst



liftC2 :: Applicative m => n ~> (Compose m n)
liftC2 = Compose <<< pure

liftC1 :: (Functor m, Applicative n) => m ~> Compose m n
liftC1 = Compose <<< fmap pure


type f ~> g = forall x. f x -> g x


liftK :: (m ~> n) -> Kleisli m :-> Kleisli n
liftK nat (Kleisli f) = Kleisli $ nat <<< f


type KIO = Kleisli IO
type PO = Compose IO (Compose [] ((,) (Product Float)))
type Ana = Kleisli PO

handleIO :: KIO :-> Ana
handleIO = liftK liftC1

handleVars :: Vars :-> Ana
handleVars = liftK (liftC2 <<< liftC1)

handleSFs :: SFs :-> Ana
handleSFs = liftK (liftC2 <<< liftC2)



getBranches :: Const MHM' a b -> MHM'
getBranches = getConst


type MHM = HM.MonoidalHashMap String
type MHM' = MHM (First (IO (ForeignPtr ())))

main ::IO ()
main = do
  (fname : tname : _) <- getArgs

  f <- tfileOpen fname

  t <- ttree f tname

  let cnv = mapConst (\(s, p) -> HM.singleton s (First p))
      Const hm =
        runFree' ana
        $ extract @(Const MHM')
          <<< runU (inj <<< ignVars @MHM')
          <<< runU (inj <<< ignSFs @MHM')
          <<< runU (inj <<< cnv <<< ptrBS)
          <<< runU (inj <<< cnv <<< ptrBV)
          <<< runU (inj <<< cnv <<< ptrBV2)


  hm' <- traverse getFirst (hm::MHM')

  _ <- either error id <$> connectBranches t hm'

  let lookup' h k = maybe (error $ "missing " ++ k) id $ HM.lookup k h
      hook = lookup' hm'
  
      (prog :: Ana () Ret) =
        runFree' ana
        $ extract
          <<< runU (inj <<< handleVars)
          <<< runU (inj <<< handleSFs)
          <<< runU (inj <<< handleIO)
          <<< runU (inj <<< readBS <<< mapL hook)
          <<< runU (inj <<< readBV <<< mapL hook)
          <<< runU (inj <<< readBV2 <<< mapL hook)


  print =<< loadEntry t 0
  
  print =<< getCompose (runKleisli prog ())

  print =<< loadEntry t 100
  
  print =<< getCompose (runKleisli prog ())


class Swap m n where
  swap :: n (m a) -> m (n a)

instance (Monad m, Monad n, Swap m n) => Monad (Compose m n) where
  Compose mna >>= f =
    let mnmnb = fmap (getCompose . f) <$> mna
        mnb = join $ fmap join . swap <$> mnmnb
    in Compose mnb

instance (Applicative m, Traversable t) => Swap m t where
  swap = sequenceA
