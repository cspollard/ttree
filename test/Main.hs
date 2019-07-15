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
import qualified Data.Vector as V
import qualified Data.HashMap.Monoidal as HM
import Analysis
import Control.Category
import Data.Profunctor
import Data.Functor.Compose
import Data.Semigroup (First(..))
import Control.Monad (join)
import Data.Monoid (Product(..))
import Foreign



-- define some convenience types
type Cuts = Kleisli Maybe -- possibility of failure

type ReadScal = Labeled2 String BS -- can read scalar branches
type ReadVec = Labeled2 String BV -- can read vector<> branches
type ReadVec2 = Labeled2 String BV2 -- can read vector<vector<>> branches

type f ~> g = forall x. f x -> g x -- natural transformation


-- reads in a variable "mu" from some event store
-- this incurs a scale factor and some syst variations
readMu :: Members rels '[ ReadScal, SFRel, VarRel String ]  => Analysis rels () Float
readMu = proc store -> do
  mu <- scalar "mu" -< store

  _ <- sf (\x -> if x < 20 then 1.1 else 0.9) -< mu

  mu' <- vars (\x -> Variation x $ HM.fromList [("muvar", First $ x*0.9)]) -< mu

  returnA -< mu'



-- a toy Analysis
-- it supports reading scalar and vector values from a tree
-- as well as systematic variations, SFs, and cuts
ana
  :: Members rels '[ ReadScal, ReadVec, VarRel String, SFRel, Cuts ]
  => Analysis rels () (Float, Vector Float)
ana = proc store -> do

  mu <- readMu -< store

  -- read jet pts from the store
  jpts <- vector "jet_pt" -< store

  -- require at least 3 jets
  _ <- cut (\v -> V.length v >= 3) -< jpts

  jpts' <- vars (\p -> Variation p $ HM.singleton "jetvar" (First $ (*0.75) <$> p)) -< jpts
  
  returnA -< (mu, jpts')



-- a physics object: IO actions, syst variations, multiplicative SFs
type PO = Compose IO (Compose (Variation String) (Compose Scaled Maybe))

-- a physics analysis from a to b: a -> PO b
type Ana = Kleisli PO


-- some convenient map types
type MHM = HM.MonoidalHashMap String
type MHM' = MHM (First (IO (ForeignPtr ())))


main ::IO ()
main = do
  (fname : tname : _) <- getArgs

  -- open the TFile and grab the TTree
  f <- tfileOpen fname

  t <- ttree f tname

  -- this section builds the list of branches that need to be read in.
  let cnv = mapC2 (\(s, p) -> HM.singleton s (First p))
      Const2 hm =
        runFree' ana
        $ extract @(Const2 MHM')
          <<< runU (inj <<< ignVars @MHM') -- ignore syst variations 
          <<< runU (inj <<< ignSFs @MHM') -- ignore SFs
          <<< runU (inj <<< ignCuts @MHM') -- ignore Cuts
          <<< runU (inj <<< cnv <<< ptrBS) -- keep pointers to scalar branches
          <<< runU (inj <<< cnv <<< ptrBV) -- keep pointers to vector branches
          <<< runU (inj <<< cnv <<< ptrBV2) -- keep pointers to vector2 branches


  -- a map of Sting to Ptrs for the branches
  hm' <- traverse getFirst (hm::MHM')

  -- branches connected to the tree!
  _ <- either error id <$> connectBranches t hm'


  let lookup' h k = maybe (error $ "missing " ++ k) id $ HM.lookup k h
      hook = lookup' hm'
  
      -- "compile" the analysis to something that can actually be run on the CPU
      (prog :: Mealy Ana () (Float, Vector Float)) =
        runFree' ana
        $ simple
          <<< extract
          <<< runU (inj <<< handleVars) -- lift Vars to a physics object
          <<< runU (inj <<< handleCuts) -- lift Cuts to a physics object
          <<< runU (inj <<< handleSFs) -- lift SFs to a physics object
          <<< runU (inj <<< handleIO) -- lift IO to a physics object
          <<< runU (inj <<< readBS <<< mapL2 hook id) -- read scalar branches
          <<< runU (inj <<< readBV <<< mapL2 hook id) -- read vector branches
          <<< runU (inj <<< readBV2 <<< mapL2 hook id) -- read vector2 branches


  -- read in a few entries and print the results of the analysis
  _ <- loadEntry t 0
  
  print =<< getCompose (runKleisli (lower prog) ())

  _ <- loadEntry t 10

  print =<< getCompose (runKleisli (lower prog) ())

  _ <- loadEntry t 100
  
  print =<< getCompose (runKleisli (lower prog) ())


-- physcis objects are Monads
class Swap m n where
  swap :: n (m a) -> m (n a)

instance (Monad m, Monad n, Swap m n) => Monad (Compose m n) where
  Compose mna >>= f =
    let mnmnb = fmap (getCompose . f) <$> mna
        mnb = join $ fmap join . swap <$> mnmnb
    in Compose mnb

instance (Applicative m, Traversable t) => Swap m t where
  swap = sequenceA


injK :: Member rels (Kleisli m) => (a -> m b) -> Analysis rels a b
injK = liftFree <<< inj <<< Kleisli


sf :: Member rels SFRel => (a -> Float) -> Analysis rels a ()
sf f = injK $ \x -> (Product $ f x, ())


vars :: Member rels (VarRel String) => (a -> Variation String b) -> Analysis rels a b
vars f = injK f


cut :: Member rels Cuts => (a -> Bool) -> Analysis rels a ()
cut f = injK $ \x -> if f x then Just () else Nothing

toConst2 :: Monoid m => p :-> Const2 m
toConst2 _ = Const2 mempty

ignVars :: Monoid m => VarRel String :-> Const2 m
ignVars = toConst2

ignSFs :: Monoid m => SFRel :-> Const2 m
ignSFs = toConst2


ignCuts :: Monoid m => Cuts :-> Const2 m
ignCuts = toConst2


-- the following lift relations in IO, Vars, SFs to relations in physics objects
handleIO :: Kleisli IO :-> Ana
handleIO = liftK liftC1

handleVars :: VarRel String :-> Ana
handleVars = liftK (liftC2 <<< liftC1)

handleSFs :: SFRel :-> Ana
handleSFs = liftK (liftC2 <<< liftC2 <<< liftC1)

handleCuts :: Cuts :-> Ana
handleCuts = liftK (liftC2 <<< liftC2 <<< liftC2)



getBranches :: Const2 MHM' a b -> MHM'
getBranches (Const2 x) = x


liftC2 :: Applicative m => n ~> (Compose m n)
liftC2 = Compose <<< pure

liftC1 :: (Functor m, Applicative n) => m ~> Compose m n
liftC1 = Compose <<< fmap pure


liftK :: (m ~> n) -> Kleisli m :-> Kleisli n
liftK nat (Kleisli f) = Kleisli $ nat <<< f
