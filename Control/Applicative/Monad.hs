{-# LANGUAGE GADTs, StandaloneDeriving, KindSignatures, RankNTypes, DeriveDataTypeable #-}

module Control.Applicative.Monad (
        AppM,
        runAppM,
        liftAppM,
        C,
        ) where

import Control.Applicative
import Data.Typeable

-- | Abstract transformer from Applicative Functor to Monad
data AppM :: (* -> *) -> * -> * where
  Return  :: a -> AppM f a
  Bind    :: (Typeable a) => f (C a) -> (C a -> AppM f b) -> AppM f b

instance Monad (AppM f) where
  return = Return
  Return a >>= k = k a
  Bind m0 k0 >>= k = Bind m0 (\ a -> k0 a >>= k)

-- | A way of running an Applicative as a Monad
runAppM :: (Typeable a, Applicative f) => AppM f (C a) -> f a
runAppM = fmap runC . runAppM' 0

runAppM' :: (Typeable a, Applicative f) => Int -> AppM f (C a) -> f (C a)
runAppM' n (Return a) = pure a
runAppM' n (Bind m k) = pure (flip ($)) <*> m <*> fmap (subst n) (runAppM' (n+1) (k (S n)))

-- | Inject an Applicative computation into the Monad.
liftAppM :: (Typeable a, Functor f) => f a -> AppM f (C a)
liftAppM app = Bind (fmap C app) Return

-------------------------------------------------------------------

runC :: C a -> a
runC (C a) = a
runC (S {}) = error "symbol in evaluation"
runC (A f a) = runC f $ runC a

-- | Abstract Container for results from computation
data C :: * -> * where
        C :: a                                  -> C a            -- value
        S :: (Typeable a) => Int                -> C a            -- symbol
        A :: (Typeable a) => C (a -> b) -> C a  -> C b
  deriving (Typeable)

instance Functor C where
  fmap f c = pure f <*> c

instance Applicative (C) where
  pure a = C a
  C f <*> C a = C (f a)
  n <*> C a = C (\ g -> g a) <*> n
  n1 <*> A n2 tx = A (C (.) <*> n1 <*> n2) tx
  n1 <*> S n     = A n1 (S n)
--  C f

-- Could use a deep embedding of Subst, and do these at one time.

subst :: (Typeable a, Typeable b) => Int -> C b -> (C a -> C b)
subst n (C a)  = const $ C a
subst n (S n') | n == n'   = \ a -> case cast a of
                                      Nothing -> error "bad subst"
                                      Just a' -> a'
               | otherwise = const $ S n'
subst n (A c1 c2) = \ a -> subst n c1 a <*> subst n c2 a

-------------------------------------------------------------------

parseCharM :: AppM App (C Char)
parseCharM = liftAppM parseChar

--exampleM :: AppM App (C (Char,Char))
exampleM = do
        c1 <- parseCharM
        parseCharM
        c2 <- parseCharM
        return (pure (,) <*> c1 <*> c2)

---------------------------------------------------------------------
-- Fake App

data App a where
  Pure :: a -> App a
  App :: App (a -> b) -> App a -> App b
  ParseChar :: App Char

instance Show (App a) where
   show (Pure _) = "pure *"
   show (App a1 a2) = "(" ++ show a1 ++ ")<*>(" ++ show a2 ++")"
   show ParseChar = "parseChar"

instance Applicative App where
  pure = Pure
  (<*>) = App

instance Functor App where
  fmap f a = pure f <*> a

parseChar :: App Char
parseChar = ParseChar

exampleA :: App (Char,Char)
exampleA = pure (,) <*> parseChar <*> parseChar

runApp :: App a -> String -> (a,String)
runApp (Pure a) str = (a,str)
runApp (App f a) str0 = (f' a', str2)
   where
           (f',str1) = runApp f str0
           (a',str2) = runApp a str1
runApp (ParseChar) (c:cs) = (c,cs)

-------------------------------------------------------------------

