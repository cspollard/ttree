module Main where

import Data.TTree
import Control.Lens
import Data.Foldable (foldrM, traverse_)

main :: IO ()
main = do c <- flip addFile "data/test.root" =<< tchain "nominal"

          c' <- foldrM addBranchF c ["mu", "weight_mc", "weight_pileup"]
          c'' <- foldrM addBranchVF c' ["jet_pt", "jet_eta", "jet_phi"]
          mc <- getEntry c'' 0

          print mc
          traverse_ printBranch $ toListOf (_Just . cBranches . traverse) mc

          -- mc' <- flip getEntry 1 <$> mc
          -- print mc'
