module Data.BVar where


import Foreign              hiding (void)
import Data.Vector
import Data.STLVec
import TTree.Internal.Common



-- could unify this into one constructor with a "Branchable"
-- constraint, but then simple Storables become annoying
data BVar s a where
  BS :: Storable a => s -> BVar s a
  BV :: Vecable a => s -> BVar s (Vector a)
  BVV :: Vecable2 a => s -> BVar s (Vector (Vector a))



runBVar :: BVar s a -> s
runBVar (BS s) = s
runBVar (BV s) = s
runBVar (BVV s) = s


-- Const s a has no constraints on a
-- => can always convert
toConst :: BVar s a -> Const s a
toConst = runBVar >>> Const


fmapBVar :: Functor f => (s -> f s') -> BVar s a -> f (BVar s' a)
fmapBVar f (BS s) = BS <$> f s
fmapBVar f (BV s) = BV <$> f s
fmapBVar f (BVV s) = BVV <$> f s


mapBVar :: (s -> s') -> BVar s a -> BVar s' a
mapBVar f = runIdentity <<< fmapBVar (Identity <<< f)


readBVar :: BVar (ForeignPtr ()) ~> IO
readBVar (BS p) = withForeignPtr p $ peek <<< castPtr
readBVar (BV p) =
  withForeignPtr p
  $ (castPtr >>> peek)
    >=> (VecPtr >>> vecFromPtr)

-- nicer in point-full form?
readBVar (BVV p) =
  withForeignPtr p
  $ (castPtr >>> peek)
    >=> (VVecPtr >>> readVV)
    >=> vecFromPtr
    >=> traverse vecFromPtr


vecFromPtr :: Vecable a => VecPtr a -> IO (Vector a)
vecFromPtr = toV >>> fmap convert
