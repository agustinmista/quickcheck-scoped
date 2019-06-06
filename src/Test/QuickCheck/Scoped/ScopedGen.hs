{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Test.QuickCheck.Scoped.ScopedGen
  ( FreqMap
  , MaxDepth
  , Label
  , LabelMap
  , ExtFreq
  , freqMap
  , ScopedGen
  , liftArbitrary
  , elements
  , growingElements
  , getEnv
  , getFreqMap
  , getMaxDepth
  , getLabels
  , depthLimit
  , freqOf
  , scoped
  -- , restricting
  , altering
  , fromEnv
  , resized
  , smaller
  , increaseLabel
  , Labelable
  , (==>)
  , listOf
  , listOf1
  , vectorOf
  , scopedListOf
  , scopedListOf1
  , scopedVectorOf
  , oneOf
  , frequency
  , externalFrequency
  , Scoped
  , runScopedGen
  , runClosedGen
  , buildGenWith
  -- Re-export generation functions for MonadGen
  , QuickCheck.GenT.MonadGen (..)
  ) where

import Data.Foldable
import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

import QuickCheck.GenT (MonadGen(..), elementsMay, growingElementsMay)
import Test.QuickCheck (Arbitrary, Gen)
import qualified Test.QuickCheck as QC

----------------------------------------
-- Generation configuration

type Label    = String
type ExtFreq  = String
type MaxDepth = Int
type FreqMap  = Map ExtFreq Int
type LabelMap = Map Label Int

data GenState env
  = GenState
    { genEnv      :: env
    , genFreqMap  :: FreqMap
    , genMaxDepth :: MaxDepth
    , genLabels   :: LabelMap
    } deriving Show

freqMap :: [(ExtFreq, Int)] -> FreqMap
freqMap = Map.fromList

----------------------------------------
-- Scoped generation monad

-- It supports:
-- * Scoping
-- * Backtracking
-- * Recursion limit
-- * Externally defined generation frequencies
-- * Tracking generated labels

newtype ScopedGen env a = ScopedGen
  { unScopedGen :: StateT (GenState env) (MaybeT QC.Gen) a
  } deriving (Functor, Applicative, Alternative,
              Monad, MonadState (GenState env))

instance MonadGen (ScopedGen env) where
  liftGen = ScopedGen . lift . lift . liftGen
  choose  = ScopedGen . lift . lift . liftGen . choose
  resize  n (ScopedGen r) = ScopedGen $ mapStateT (mapMaybeT (resize n)) r
  variant k (ScopedGen r) = ScopedGen $ mapStateT (mapMaybeT (variant k)) r
  sized f = ScopedGen $ StateT $ MaybeT . gen
    where gen env = sized (runMaybeT . flip runStateT env . (unScopedGen . f))

-- | Lift QuickCheck arbitrary into a ScopedGen
liftArbitrary :: Arbitrary a => ScopedGen env a
liftArbitrary = liftGen QC.arbitrary

elements :: Foldable f => f a -> ScopedGen env a
elements = elementsMay . toList >=> maybe empty pure

growingElements :: Foldable f => f a -> ScopedGen env a
growingElements = growingElementsMay . toList >=> maybe empty pure

----------------------------------------
-- Generation state projections

-- | Get the current environment
getEnv :: ScopedGen env env
getEnv = gets genEnv

-- | Get the current recursion limit
getMaxDepth :: ScopedGen env MaxDepth
getMaxDepth = gets genMaxDepth

-- | Get the external generation frequencies
getFreqMap :: ScopedGen env FreqMap
getFreqMap = gets genFreqMap

getLabels :: ScopedGen env LabelMap
getLabels = gets genLabels

-- | Check wether we reached the depth limit
depthLimit :: ScopedGen env Bool
depthLimit = (0==) <$> getMaxDepth

-- | Get the external generation frequency of a given label
freqOf :: Label -> ScopedGen env Int
freqOf x = Map.findWithDefault 1 x <$> getFreqMap

----------------------------------------
-- Scoping combinators

-- | Run a scoped generation in an isolated environment
scoped :: ScopedGen env a -> ScopedGen env a
scoped gen = do
  env <- getEnv
  res <- gen
  modify (\st -> st { genEnv = env })
  return res

-- | Modify the current environment based on the output of a generator
altering :: ScopedGen env a -> (a -> env -> env) -> ScopedGen env a
altering gen f = do
  x <- gen
  modify (\st -> st { genEnv = f x (genEnv st) })
  return x

-- | Pick a random element from the environment using a given generator
fromEnv :: (env -> ScopedGen env a) -> ScopedGen env a
fromEnv gen = getEnv >>= gen

----------------------------------------
-- Depth limiting combinators

-- | Run a generator with a modified depth bound
resized :: (MaxDepth -> MaxDepth) -> ScopedGen env a -> ScopedGen env a
resized f gen = do
  limit <- getMaxDepth
  modify (\st -> st { genMaxDepth = max 0 (f (genMaxDepth st)) })
  res <- gen
  modify (\st -> st { genMaxDepth = limit })
  return res

-- | Run a generator with a maximum depth of one less the that current one
smaller :: ScopedGen env a -> ScopedGen env a
smaller = resized (subtract 1)

----------------------------------------
-- Label combinators

increaseLabel :: Label -> ScopedGen env ()
increaseLabel l = modify $ \st ->
  st { genLabels = Map.insertWith (+) l 1 (genLabels st) }

class Labelable a where
  label :: a -> Label -> a

instance Labelable (ScopedGen env a) where
  label g l = increaseLabel l >> g

instance Labelable (ScopedGen env a -> ScopedGen env a) where
  label g l = \r -> g r `label` l

(==>) :: Labelable b => Label -> b -> (Label, b)
l ==> g = (l, g `label` l)

infix 2 ==>

----------------------------------------
-- | QuickCheck's list generating combinators.

vectorOf :: Int -> ScopedGen env a -> ScopedGen env [a]
vectorOf n gen = go n
  where
    go 0 = return []
    go k = do
      v <- gen
      vs <- go (k-1)
      return (v : vs)

listOf :: ScopedGen env a -> ScopedGen env [a]
listOf gen = sized $ \n -> do
  k <- choose (0, n)
  vectorOf k gen

listOf1 :: ScopedGen env a -> ScopedGen env [a]
listOf1 gen = sized $ \n -> do
  k <- choose (1, max 1 n)
  vectorOf k gen

----------------------------------------
-- | Scoped list combinators

-- | Generation of "sequential" values, where scoping goes deeper from
-- left to right
scopedVectorOf :: Int -> ScopedGen env a -> ScopedGen env [a]
scopedVectorOf n gen = go n
  where
    go 0 = return []
    go k = do
      v <- gen
      vs <- scoped (go (k-1))
      return (v : vs)

scopedListOf :: ScopedGen env a -> ScopedGen env [a]
scopedListOf gen = sized $ \n -> do
  k <- choose (0, n)
  scopedVectorOf k gen

scopedListOf1 :: ScopedGen env a -> ScopedGen env [a]
scopedListOf1 gen = sized $ \n -> do
  k <- choose (1, max 1 n)
  scopedVectorOf k gen

----------------------------------------
-- | Random choice combinators with failure recovery

-- | Picks a generator from a list of generators with uniform probability.
-- In case of fail, retries recursively among the rest.
oneOf :: [ScopedGen env a] -> ScopedGen env a
oneOf []  = empty
oneOf [g] = g
oneOf gs  = do
  n <- choose (0, length gs - 1)
  let (hd, g : tl) = splitAt n gs
  g <|> oneOf (hd ++ tl)

-- | Picks a generator from a list of generators with explicit frequencies.
-- In case of fail, retries recursively among the rest.
frequency :: [(Int, ScopedGen env a)] -> ScopedGen env a
frequency [ ] = empty
frequency [g] = snd g
frequency xs0 = choose (1, sum (map fst xs0)) >>= pick 0 xs0
  where
    pick i ((k,x):xs) n
      | n <= k = x <|> frequency (delete i xs0)
      | otherwise = pick (n+1) xs (n-k)
    pick _ _ _ = empty
    delete i xs = take i xs ++ drop (i+1) xs

-- | Picks a generator from a list of externalFrequency generators.
-- Generation frequencies can be later assigned from outside.
-- In case of fail, retries recursively among the rest.
externalFrequency :: [(ExtFreq, ScopedGen env a)] -> ScopedGen env a
externalFrequency xs = do
  let mkTuple (l, g) = do
        f <- freqOf l
        return (f, g)
  freqGens <- mapM mkTuple xs
  frequency freqGens

----------------------------------------
-- | Generation of Scoped values

-- | We can tag a well-scoped value via a wrapper
newtype Scoped a = Scoped { unScoped :: a }
  deriving (Eq, Ord, Show, Read, Functor)

-- | Run the scoped generator with an initial context
runScopedGen :: env -> FreqMap -> MaxDepth
             -> ScopedGen env a -> Gen (Maybe (Scoped a, LabelMap))
runScopedGen env freqs depth g = do
  ma <- runMaybeT (runStateT (unScopedGen g) (GenState env freqs depth mempty))
  return (fmap (Scoped *** genLabels) ma)

runClosedGen :: Monoid env => FreqMap -> MaxDepth -> ScopedGen env a
             -> Gen (Maybe (Scoped a, LabelMap))
runClosedGen = runScopedGen mempty

----------------------------------------
-- | Generation patterns

buildGenWith :: Functor f
             => ([f (ScopedGen env a)] -> ScopedGen env a)
             -> [f (ScopedGen env a)]
             -> [f (ScopedGen env a -> ScopedGen env a)]
             -> ScopedGen env a
buildGenWith method terms recs = gen
  where
    gen = do
      limit <- depthLimit
      if limit
        then method terms
        else method (terms <> (fmap . fmap) ($ smaller gen) recs)
