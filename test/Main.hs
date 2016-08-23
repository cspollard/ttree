module Main where

import Data.TTree
import Foreign
import Control.Lens
import Control.Monad ((>=>))
import Data.Foldable (forM_, foldlM, traverse_)

main :: IO ()
main = do c <- flip addFile "data/test.root" =<< tchain "nominal"

          c' <- foldlM addBranchF c ["mu", "weight_mc", "weight_pileup"]
          c'' <- foldlM addBranchVF c' ["jet_pt", "jet_eta", "jet_phi"]
          mc <- getEntry c'' 0

          print mc
          traverse_ printBranch $ toListOf (_Just . cBranches . traverse) mc

          -- TODO
          -- free mallocs
