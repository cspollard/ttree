{-# LANGUAGE FlexibleContexts  #-}

module TTree.Branch where


import Foreign              hiding (void)
import Data.Vector
import TTree.STLVec
import TTree.Internal.Common
import Control.Monad ((>=>))
import Analysis.Free
import Analysis.Const



scalar
  :: (Member arrs (L s BS), Storable a)
  => s -> Analysis arrs () a
scalar s = liftFree <<< inj $ L s BS
{-# INLINE scalar  #-}


vector
  :: (Member arrs (L s BV), Vecable a)
  => s -> Analysis arrs () (Vector a)
vector s = liftFree <<< inj $ L s BV
{-# INLINE vector  #-}


vector2
  :: (Member arrs (L s BV2), Vecable2 a)
  => s -> Analysis arrs () (Vector (Vector a))
vector2 s = liftFree <<< inj $ L s BV2
{-# INLINE vector2  #-}


-- a labeled relation
data L s p a b = L !s !(p a b)


mapL :: (s -> s') -> L s p :-> L s' p
mapL f (L s p) = L (f s) p
{-# INLINE mapL  #-}


labeledToConst :: L s p :-> Const s
labeledToConst (L s _) = Const s
{-# INLINE labeledToConst  #-}


data BS a b where
  BS :: Storable a => BS () a

data BV a b where
  BV :: Vecable a => BV () (Vector a)

data BV2 a b where
  BV2 :: Vecable2 a => BV2 () (Vector (Vector a))


readBS :: L VFP BS :-> Kleisli IO
readBS (L p BS) =
  Kleisli $ \() ->
    withForeignPtr p $ peek <<< castPtr
{-# INLINE readBS  #-}


readBV :: L VFP BV :-> Kleisli IO
readBV (L p BV) =
  Kleisli $ \() ->
    withForeignPtr p
    $ (castPtr >>> peek)
      >=> (VecPtr >>> vecFromPtr)
{-# INLINE readBV  #-}


readBV2 :: L VFP BV2 :-> Kleisli IO
readBV2 (L p BV2) =
  Kleisli $ \() ->
    withForeignPtr p
    $ (castPtr >>> peek)
      >=> (VVecPtr >>> readVV)
      >=> vecFromPtr
      >=> traverse vecFromPtr
{-# INLINE readBV2  #-}


vecFromPtr :: Vecable a => VecPtr a -> IO (Vector a)
vecFromPtr = toV >>> fmap convert
{-# INLINE vecFromPtr  #-}


-- for this one I need access to the type b, so I can't use :-> notation.
ptrBS :: forall a b. L String BS a b -> Const (String, IO VFP) a b
ptrBS (L s BS) =
  -- we take responsibility for freeing scalars.
  let p = castForeignPtr <$> mallocForeignPtr @b
  in Const (s, p)
{-# INLINE ptrBS  #-}


vptr :: IO VFP
vptr = castForeignPtr <$> mallocForeignPtr @(Ptr ())
{-# INLINE vptr  #-}


ptrBV :: L String BV :-> Const (String, IO VFP)
ptrBV = mapL (,vptr) >>> labeledToConst
{-# INLINE ptrBV  #-}


ptrBV2 :: L String BV2 :-> Const (String, IO VFP)
ptrBV2 = mapL (,vptr) >>> labeledToConst
{-# INLINE ptrBV2  #-}
