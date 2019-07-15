{-# LANGUAGE FlexibleContexts #-}

module TTree.Branch where


import Prelude hiding (id, (.))
import Foreign              hiding (void)
import Data.Vector
import TTree.STLVec
import TTree.Internal.Common
import Control.Monad ((>=>))
import Analysis



scalar
  :: (Member arrs (Labeled2 s BS), Storable a)
  => s -> Analysis arrs () a
scalar s = liftFree <<< inj $ Labeled2 s BS
{-# INLINE scalar  #-}


vector
  :: (Member arrs (Labeled2 s BV), Vecable a)
  => s -> Analysis arrs () (Vector a)
vector s = liftFree <<< inj $ Labeled2 s BV
{-# INLINE vector  #-}


vector2
  :: (Member arrs (Labeled2 s BV2), Vecable2 a)
  => s -> Analysis arrs () (Vector (Vector a))
vector2 s = liftFree <<< inj $ Labeled2 s BV2
{-# INLINE vector2  #-}


data BS a b where
  BS :: Storable a => BS () a

data BV a b where
  BV :: Vecable a => BV () (Vector a)

data BV2 a b where
  BV2 :: Vecable2 a => BV2 () (Vector (Vector a))


readBS :: Labeled2 VFP BS :-> Kleisli IO
readBS (Labeled2 p BS) =
  Kleisli $ \() ->
    withForeignPtr p $ peek <<< castPtr
{-# INLINE readBS  #-}


readBV :: Labeled2 VFP BV :-> Kleisli IO
readBV (Labeled2 p BV) =
  Kleisli $ \() ->
    withForeignPtr p
    $ (castPtr >>> peek)
      >=> (VecPtr >>> vecFromPtr)
{-# INLINE readBV  #-}


readBV2 :: Labeled2 VFP BV2 :-> Kleisli IO
readBV2 (Labeled2 p BV2) =
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
ptrBS :: forall a b. Labeled2 String BS a b -> Const2 (String, IO VFP) a b
ptrBS (Labeled2 s BS) =
  -- we take responsibility for freeing scalar pointers.
  let p = castForeignPtr <$> mallocForeignPtr @b
  in Const2 (s, p)
{-# INLINE ptrBS  #-}


vptr :: IO VFP
vptr = castForeignPtr <$> mallocForeignPtr @(Ptr ())
{-# INLINE vptr  #-}


ptrBV :: Labeled2 String BV :-> Const2 (String, IO VFP)
ptrBV = mapL2 (,vptr) id >>> forgetL2
{-# INLINE ptrBV  #-}


ptrBV2 :: Labeled2 String BV2 :-> Const2 (String, IO VFP)
ptrBV2 = mapL2 (,vptr) id >>> forgetL2
{-# INLINE ptrBV2  #-}
