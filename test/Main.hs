module Main where

import Data.TTree
import Control.Lens
import Data.Foldable (foldrM)
import Conduit

main :: IO ()
main = do c <- tchain "nominal"
          addFile c "data/test.root"
          c' <- flip (foldrM addBranchF) ["mu", "weight_mc", "weight_pileup"] 
                =<< flip (foldrM addBranchVF) ["jet_pt", "jet_eta", "jet_phi"]
                =<< flip (foldrM addBranchD) ["weight_EW"]
                =<< flip (foldrM addBranchVC) ["el_isTight"]
                =<< flip (foldrM addBranchL) ["eventNumber"]
                =<< foldrM addBranchU c ["runNumber"]

          runChain c' $$ takeC 99999 =$= mapM_C print
