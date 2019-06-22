module Data.TBranch where


import Foreign              hiding (void)
import Data.Vector
import Data.STLVec
import TTree.Internal.Common
import Control.Monad ((<=<))


data BVar s a where
  BS :: Storable a => s -> BVar s a
  BV :: Vecable a => s -> BVar s (Vector a)
  BVV :: Vecable2 a => s -> BVar s (Vector (Vector a))



unBVar :: BVar s a -> s
unBVar (BS s) = s
unBVar (BV s) = s
unBVar (BVV s) = s


toConst :: BVar s a -> Const s a
toConst = unBVar >>> Const


fmapBVar :: Functor f => (s -> f s') -> BVar s a -> f (BVar s' a)
fmapBVar f (BS s) = BS <$> f s
fmapBVar f (BV s) = BV <$> f s
fmapBVar f (BVV s) = BVV <$> f s


mapBVar :: (s -> s') -> BVar s a -> BVar s' a
mapBVar f = runIdentity <<< fmapBVar (Identity <<< f)


vecFromPtr :: Vecable a => VecPtr a -> IO (Vector a)
vecFromPtr = fmap convert <<< toV


readBVar :: BVar (ForeignPtr ()) ~> IO
readBVar (BS p) = withForeignPtr p $ peek <<< castPtr
readBVar (BV p) =
  withForeignPtr p $ (vecFromPtr <<< VecPtr) <=< (peek <<< castPtr)

readBVar (BVV p) = withForeignPtr p $ \p' -> do
  vvp <- (readVV <<< VVecPtr) =<< (peek <<< castPtr) p'
  vecp <- vecFromPtr vvp
  traverse vecFromPtr vecp
