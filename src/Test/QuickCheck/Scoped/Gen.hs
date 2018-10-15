{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.QuickCheck.Scoped.Gen
  ( ScopedGen
  , getContext
  , liftArbitrary
  , scoped
  , restricted
  , oneOf
  , frequency
  , elements
  , growingElements
  , listOf
  , listOf1
  , vectorOf
  , fromContext
  , Scoped
  , runScopedGen
  , runEmptyScopedGen
  , AtomGen
  , RecGen
  , sizedUniformGen
  , sizedFreqsGen
  -- Re-export generation functions for MonadGen
  , QuickCheck.GenT.MonadGen (..)
  -- Re-export QuickCheck basics
  , Test.QuickCheck.Gen
  , Test.QuickCheck.Arbitrary
  ) where

import Data.Foldable

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import QuickCheck.GenT (MonadGen(..), elementsMay, growingElementsMay)
import Test.QuickCheck (Arbitrary, Gen)
import qualified Test.QuickCheck as QC


-- | A random generator with a scoping context and failure recovery
newtype ScopedGen ctx a
  = ScopedGen { unScopedGen :: ReaderT ctx (MaybeT QC.Gen) a }
  deriving ( Functor, Applicative, Alternative, Monad, MonadReader ctx)

instance MonadGen (ScopedGen ctx) where
  liftGen = ScopedGen . lift . lift . liftGen
  choose  = ScopedGen . lift . lift . liftGen . choose
  resize  n (ScopedGen r) = ScopedGen $ mapReaderT (mapMaybeT (resize n)) r
  variant k (ScopedGen r) = ScopedGen $ mapReaderT (mapMaybeT (variant k)) r
  sized f = ScopedGen $ ReaderT $ MaybeT . gen
    where gen ctx = sized (runMaybeT . flip runReaderT ctx . (unScopedGen . f))


-- | Lift QuickCheck arbitrary into a ScopedGen
liftArbitrary :: Arbitrary a => ScopedGen ctx a
liftArbitrary = liftGen QC.arbitrary

-- | Returns the current context
getContext :: ScopedGen ctx ctx
getContext = ask

-- | Picks a random element from the context if there exists any
fromContext :: Foldable f => ScopedGen (f v) v
fromContext = getContext >>= elementsMay . toList >>= maybe empty pure

-- | Run a scoped generation within a modified context
scoped :: (ctx -> ctx) -> ScopedGen ctx a -> ScopedGen ctx a
scoped = local

-- | Run a scoped generation within a restricted context
restricted :: (Applicative f, Foldable f, Monoid (f v))
           => (v -> Bool) -> ScopedGen (f v) a -> ScopedGen (f v) a
restricted keep = scoped $ foldMap (\v -> if keep v then pure v else mempty)

-- | Scoped generation of "sequential" values
vectorOf :: Int -> (a -> ctx -> ctx) -> ScopedGen ctx a -> ScopedGen ctx [a]
vectorOf n f gen = go n
  where
    go 0 = return []
    go k = do
      v <- gen
      vs <- local (f v) (go (k-1))
      return (v : vs)

listOf :: (a -> ctx -> ctx) -> ScopedGen ctx a -> ScopedGen ctx [a]
listOf f gen = sized $ \n -> do
  k <- choose (0, n)
  vectorOf k f gen

listOf1 :: (a -> ctx -> ctx) -> ScopedGen ctx a -> ScopedGen ctx [a]
listOf1 f gen = sized $ \n -> do
  k <- choose (1, max 1 n)
  vectorOf k f gen

-- | Picks a generator from a list of generators with uniform probability.
-- In case of fail, retries recursively among the rest.
oneOf :: [ScopedGen ctx a] -> ScopedGen ctx a
oneOf [ ] = empty
oneOf [g] = g
oneOf gs  = do n <- choose (0, length gs - 1)
               let (hd, g : tl) = splitAt n gs
               g <|> oneOf (hd ++ tl)

-- | Picks a generator from a list of generators with explicit frequencies.
-- In case of fail, retries recursively among the rest.
frequency :: [(Int, ScopedGen ctx a)] -> ScopedGen ctx a
frequency [ ] = empty
frequency [g] = snd g
frequency xs0  = choose (1, sum (map fst xs0)) >>= pick 0 xs0
  where
    pick i ((k,x):xs) n
      | n <= k = x <|> frequency (delete i xs0)
      | otherwise = pick (n+1) xs (n-k)
    pick _ _ _ = empty
    delete i xs = take i xs ++ drop (i+1) xs

elements :: [a] -> ScopedGen ctx a
elements = elementsMay >=> maybe empty pure

growingElements :: [a] -> ScopedGen ctx a
growingElements = growingElementsMay >=> maybe empty pure


-- | We can tag a well-scoped value via a wrapper
newtype Scoped a
  = Scoped { unScoped :: a }
  deriving (Eq, Ord, Show, Read, Functor)

-- | Run the scoped generator with an initial context
runScopedGen :: ctx -> ScopedGen ctx a -> Gen (Maybe (Scoped a))
runScopedGen ctx g = do
  ma <- runMaybeT (runReaderT (unScopedGen g) ctx)
  return (Scoped <$> ma)

runEmptyScopedGen :: Monoid ctx => ScopedGen ctx a -> Gen (Maybe (Scoped a))
runEmptyScopedGen = runScopedGen mempty


------------------------------------------------
-- | Build generators following a common pattern

type AtomGen = ScopedGen
type RecGen ctx a = ((Int -> Int) -> ScopedGen ctx a) -> ScopedGen ctx a

sizedFreqsGen :: [(Int, AtomGen ctx a)] -> [(Int, RecGen ctx a)] -> ScopedGen ctx a
sizedFreqsGen atoms recs = sized gen
  where
    gen 0 = frequency atoms
    gen n = frequency (map (y n) recs ++ atoms)
    y n (freq, g) = (freq, g (\f -> gen (f n)))

sizedUniformGen :: [AtomGen ctx a] -> [RecGen ctx a] -> ScopedGen ctx a
sizedUniformGen atoms recs = sized gen
  where
    gen 0 = oneOf atoms
    gen n = oneOf (map (y n) recs ++ atoms)
    y n g = g (\f -> gen (f n))
