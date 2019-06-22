{-# LANGUAGE ForeignFunctionInterface  #-}

module Data.TTree where

import Foreign
import Foreign.C.Types
import Data.Foldable (traverse_)
import Foreign.C.String
import Data.TBranch
import Data.TFile
import qualified Data.HashMap.Monoidal as HM
import TTree.Internal.Common
import Data.Semigroup (First(..))


-- void pointer
type VP = Ptr ()
type VFP = ForeignPtr ()


foreign import ccall "ttreeC.h ttree" _ttree
  :: TFile -> CString -> IO VP
foreign import ccall "ttreeC.h ttreeLoadTree" _ttreeLoadTree
  :: VP -> Int -> IO CLong
foreign import ccall "ttreeC.h ttreeGetBranchEntry" _ttreeGetBranchEntry
  :: VP -> CString -> Int -> Ptr a -> IO Int
foreign import ccall "ttreeC.h &ttreeFree" _ttreeFree
  :: FunPtr (VP -> IO ())


type MHM = HM.MonoidalHashMap

data TTree = TTree !VP



withPtr :: BVar String ~> BVar (String, First (IO VFP))
withPtr = \case

  -- for scalars we explicitly take responsibility for freeing
  (BS s :: BVar String a) ->
    BS (s, pure $ castForeignPtr <$> mallocForeignPtr @a)

  -- for vectors we do not take responsibility for freeing
  BV s ->
    BV (s, pure $ castForeignPtr <$> mallocForeignPtr @(Ptr ()))
  BVV s ->
    BVV (s, pure $ castForeignPtr <$> mallocForeignPtr @(Ptr ()))


runWithPtrs :: MHM String VFP -> BVar String ~> BVar (Either String VFP)
runWithPtrs hm = mapBVar $ \s ->
  case HM.lookup s hm of
    Nothing -> Left $ "missing pointer for BVar \"" ++ s ++ "\""
    Just p -> Right p
  

connectBranches :: VP -> MHM String VFP -> Comp IO (Either String) ()
connectBranches ttreeptr hm = traverse_ go $ HM.toList hm
  where
    go (k, p) =
      Comp <<< withCString k $ \k' ->
        withForeignPtr p $ \p' -> do
          n <- _ttreeGetBranchEntry ttreeptr k' 0 p'
          case n of
            0 -> return <<< Left $ "missing branch \"" ++ k ++ "\""
            _ -> return $ Right ()


ttree :: TFile -> String -> IO TTree
ttree f tn = do
  tp <- withCString tn (_ttree f)
  return $ TTree tp


isNullTree :: TTree -> Bool
isNullTree (TTree p) = p == nullPtr



loadEntry :: TTree -> Int -> IO Bool
loadEntry (TTree p) i = (>= 0) <$> _ttreeLoadTree p i


-- TODO
-- -- this really should be of type Int -> TTree -> (ExceptT TreeError m) a
-- -- but this is isomorphic, no?
-- type TreeRead m = ReaderT Int (StateT TTree m)
-- 
-- 
-- readEntry :: Monad m => TreeRead m Int
-- readEntry = ask
-- 
-- 
-- -- note: this assumes that once a branch has been requested
-- -- that we want to load it for *EVERY EVENT*
-- readBranchMaybe
--   :: (MonadIO m, Branchable a, Storable (HeapType a))
--   => String -> TreeRead m (Maybe a)
-- readBranchMaybe s = do
--   i <- ask
--   t <- get
--   case s `HM.lookup` ttreeBranches t of
--     -- we've already read this branch at least once: don't alloc a
--     -- new pointer
--     Just p  -> fmap Just . liftIO $ withForeignPtr (castForeignPtr p) fromB
-- 
--     -- this is the first time we've accessed this branch: alloc a
--     -- new pointer
--     Nothing -> do
--       p <- liftIO $ newForeignPtr_ =<< calloc
--       modify . const
--         $ t { ttreeBranches = HM.insert s (castForeignPtr p) (ttreeBranches t) }
--       n <-
--         liftIO
--           $ withCString s
--           $ \s' -> withForeignPtr (ttreePtr t)
--           $ \tp' -> withForeignPtr p
--           $ \p' -> _ttreeGetBranchEntry tp' s' i p'
-- 
--       if n > 0
--          then fmap Just . liftIO $ withForeignPtr p fromB
--          else return Nothing
-- 
-- 
-- data TTreeException = EndOfTTree | TBranchReadFailure String
--   deriving (Typeable, Show)
-- 
-- instance Exception TTreeException where
-- 
-- 
-- readBranch
--   :: ( MonadIO m, MonadThrow m
--      , Branchable a, Storable (HeapType a))
--   => String -> TreeRead m a
-- readBranch s = do
--   m <- readBranchMaybe s
--   case m of
--     Nothing -> do
--       i <- ask
--       throwM . TBranchReadFailure
--         $ "unable to read branch " ++ s ++ " for event number " ++ show i
--     Just x  -> return x
-- 
-- 
-- class FromTTree a where
--     fromTTree :: (MonadIO m, MonadThrow m) => TreeRead m a
-- 
-- 
-- 
-- 
-- pipeTTree
--   :: (MonadIO m, MonadState TTree m)
--   => ReaderT Int m b
--   -> Pipe Int b m ()
-- pipeTTree f = do
--   i <- await
--   mx <- lift $ readTTreeEntry f i
--   case mx of
--     Just x -> do
--       yield x
--       pipeTTree f
--     Nothing -> return ()
