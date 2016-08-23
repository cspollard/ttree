module Main where

import Data.TTree
import Foreign
import Control.Lens
import Control.Monad ((>=>))
import Data.Foldable (forM_, foldlM, traverse_)

main :: IO ()
main = do c <- flip addFile "data/test.root" =<< tchain "nominal"

          c' <- foldlM addBranchF c ["mu", "weight_mc", "weight_pileup"]
          mc <- getEntry c' 0

          print mc
          traverse_ (peek >=> print) $ toListOf (_Just . cBranches . traverse . _TBFloat) mc

          -- p <- peek vptr
          -- print =<< peekArray (vectorSizeF p) (vectorDataF p)

          -- TODO
          -- free mallocs
