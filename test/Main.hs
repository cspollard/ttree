module Main where

import Data.TTree
import Data.Foldable (foldrM)
import Conduit
import System.Environment (getArgs)

main :: IO ()
main = do (cn:fns) <- getArgs
          c <-  tchain cn
          mapM_ (addFile c) fns
          c' <- flip (foldrM addBranchF) ["mu", "weight_mc", "weight_pileup"] 
                =<< flip (foldrM addBranchVF) ["jet_pt", "jet_eta", "jet_phi"]
                =<< flip (foldrM addBranchVC) ["el_isTight"]
                =<< flip (foldrM addBranchL) ["eventNumber"]
                =<< foldrM addBranchU c ["runNumber"]

          runChain c' $$ mapM_C print
