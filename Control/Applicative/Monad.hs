{-# LANGUAGE GADTs, StandaloneDeriving, KindSignatures, RankNTypes, DeriveDataTypeable #-}

module Control.Applicative.Monad (
        AppM,
        runAppM,
        liftAppM,
        C,
        ) where

import Control.Applicative
import Data.Typeable
import Data.Maybe

-- | Abstract transformer from Applicative Functor to Monad
data AppM :: * -> (* -> *) -> * -> * where
  Return  :: a -> AppM s f a
  Bind    :: (Typeable a) => f (C s a) -> (C s a -> AppM s f b) -> AppM s f b

instance Monad (AppM s f) where
  return = Return
  Return a >>= k = k a
  Bind m0 k0 >>= k = Bind m0 (\ a -> k0 a >>= k)

-- | A way of running an Applicative as a Monad. Uses the rank-2 trick to make this sound.

runAppM :: (Typeable a, Applicative f) => (forall s . AppM s f (C s a)) -> f a
runAppM m = fmap runC (runAppM' 0 m)

runAppM' :: (Typeable a, Applicative f) => Int -> AppM s f (C s a) -> f (C s a)
runAppM' n (Return a) = pure a
runAppM' n (Bind m k) = pure (flip ($)) <*> m <*> fmap (subst n) (runAppM' (n+1) (k (S n)))

-- | Inject an Applicative computation into the Monad.
liftAppM :: (Typeable a, Functor f) => f a -> AppM s f (C s a)
liftAppM app = Bind (fmap pure app) Return

-------------------------------------------------------------------

runC :: C s a -> a
runC (C a) = a
runC (S {}) = error "symbol in evaluation"
runC (A f a) = runC f $ runC a

-- | Abstract Container for results from computation
data C :: * -> * -> * where
        C :: a                                      -> C s a            -- value
        S :: (Typeable a) => Int                    -> C s a            -- symbol
        A :: (Typeable a) => C s (a -> b) -> C s a  -> C s b
  deriving (Typeable)

instance Functor (C s) where
  fmap f c = pure f <*> c

instance Applicative (C s) where
  pure a = C a
  C f <*> C a = C (f a)
  n <*> C a = C (\ g -> g a) <*> n
  n1 <*> A n2 tx = A (C (.) <*> n1 <*> n2) tx
  n1 <*> S n     = A n1 (S n)
--  C f

castC :: (Typeable a, Typeable b) => C s a -> C s b
castC = fmap (fromMaybe (error "bad subst") . cast)

-- Could use a deep embedding of Subst, and do these at one time.
subst :: (Typeable a, Typeable b) => Int -> C s b -> (C s a -> C s b)
subst n (C a)  = const $ C a
subst n (S n') | n == n'   = \ a -> castC a
               | otherwise = const $ S n'
subst n (A c1 c2) = \ a -> subst n c1 a <*> subst n c2 a

-------------------------------------------------------------------
