{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.QuickCheck.Scoped.ScopedGen
  ( Label(Label)
  , label
  , getLabel
  , getValue
  , FreqMap
  , freqMap
  , ScopedGen
  , MaxDepth
  , LabelMap
  , liftArbitrary
  , elements
  , growingElements
  , suchThat
  , getEnv
  , setEnv
  , alterEnv
  , updateEnv
  , getFreqMap
  , setFreqMap
  , freqOf
  , alterFreq
  , getMaxDepth
  , getLabels
  , depthLimit
  , scoped
  , (|-)
  , fromEnv
  , resized
  , smaller
  , bigger
  , runLabeledGen
  , FromLabeled
  , Labeled((:=))
  , vectorOf
  , listOf
  , listOf1
  , genFail
  , oneOf
  , frequency
  , labeledFrequency
  , Scoped(..)
  , runScopedGen
  , runScopedGen'
  , buildGenWith
  , buildGenWith'
  -- Re-export generation functions for MonadGen
  , QuickCheck.GenT.MonadGen (..)
  -- Re-export some QuickCheck utilities
  , QC.generate
  , QC.Arbitrary(..)
  ) where

import FastString
import Data.Proxy
import GHC.OverloadedLabels
import GHC.TypeLits

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
-- Labeled values

newtype Label
  = Label FastString
  deriving (Eq, Ord)

instance Show Label where
  show (Label x) = '#' : unpackFS x

label :: String -> Label
label = Label . fsLit

data Labeled l a
  = l := a
  deriving (Show, Functor)

infix 2 :=

getLabel :: Labeled l a -> l
getLabel (l := _) = l

getValue :: Labeled l a -> a
getValue (_ := a) = a

----------------------------------------
-- Generation configuration

type Freq     = Int
type Depth    = Int
type MaxDepth = Depth
type FreqMap  = Map Label (Depth -> Freq)
type LabelMap = Map Label Int

data GenState env
  = GenState
    { genEnv      :: env
    , genFreqMap  :: FreqMap
    , genMaxDepth :: MaxDepth
    , genLabels   :: LabelMap
    } 

freqMap :: [Labeled Label (Depth -> Int)] -> FreqMap
freqMap = Map.fromList . map (getLabel &&& getValue)

----------------------------------------
-- Scoped generation monad

-- It supports:
-- * Scoping
-- * Failure and backtracking
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

suchThat :: ScopedGen env a -> (env -> a -> Bool) -> ScopedGen env a
suchThat gen f = do
  env <- getEnv
  a <- gen
  if f env a
    then return a
    else suchThat gen f 

----------------------------------------
-- | Interaction with the generation state

-- | Environment

-- | Access directly to the generation environment
getEnv :: ScopedGen env env
getEnv = gets genEnv

setEnv :: env -> ScopedGen env ()
setEnv env = modify $ \st -> st { genEnv = env } 

updateEnv :: (env -> env) -> ScopedGen env ()
updateEnv f = modify $ \st -> st { genEnv = f (genEnv st) } 

-- | Generation frequencies

-- | Access directly to the external generation frequencies
getFreqMap :: ScopedGen env FreqMap
getFreqMap = gets genFreqMap

setFreqMap :: FreqMap -> ScopedGen env ()
setFreqMap freqs = modify $ \st -> st { genFreqMap = freqs }

-- | Get the external generation frequency of a given label
-- Defaults to 1
freqOf :: Label -> ScopedGen env Int
freqOf lbl = do
  depth <- getMaxDepth
  freq <- Map.findWithDefault (const 1) lbl <$> getFreqMap
  return (freq depth)

-- | Alter the generation frequency of a given label
alterFreq :: (Int -> Int) -> Label -> ScopedGen env ()
alterFreq f lbl = do
  modify $ \st -> st { genFreqMap = Map.alter (fmap (f .)) lbl (genFreqMap st) }

-- | Recursion limit

-- | Get the current recursion limit
getMaxDepth :: ScopedGen env MaxDepth
getMaxDepth = gets genMaxDepth

-- | Check wether we reached the depth limit
depthLimit :: ScopedGen env Bool
depthLimit = (0==) <$> getMaxDepth

-- | Generated labels

-- | Get the generated labels count
getLabels :: ScopedGen env LabelMap
getLabels = gets genLabels


----------------------------------------
-- Scoping combinators

-- | Run a scoped generation in an isolated environment
scoped :: ScopedGen env a -> ScopedGen env a
scoped gen = do
  env <- getEnv
  res <- gen
  setEnv env
  return res

-- | Modify the current environment based on the output of a generator
alterEnv :: ScopedGen env a -> (a -> env -> env) -> ScopedGen env a
alterEnv gen f = do
  x <- gen
  updateEnv (f x)
  return x

(|-) :: ScopedGen env a -> (a -> env -> env) -> ScopedGen env a
(|-) = alterEnv

infix 5 |-

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

-- | Run a generator with a maximum depth of one more the that current one
bigger :: ScopedGen env a -> ScopedGen env a
bigger = resized (+1)

----------------------------------------
-- Label combinators

runLabeledGen :: Labeled Label (ScopedGen env a) -> ScopedGen env a
runLabeledGen (l := gen) = do
  let inc k = Map.insertWith (+) k 1
  modify $ \st -> st { genLabels = inc l (genLabels st) }
  gen

class FromLabeled l a where
  unlabel  :: Labeled l a -> a

instance FromLabeled Label (ScopedGen env a) where
  unlabel lg = runLabeledGen lg

instance FromLabeled Label (ScopedGen env a -> ScopedGen env a) where
  unlabel  (l := g) = \r -> runLabeledGen (l := g r)

instance KnownSymbol symbol => IsLabel symbol Label where
  fromLabel = Label . fsLit $ symbolVal (Proxy @symbol)

----------------------------------------
-- | Scoped list combinators

-- | Generation of "sequential" values, where scoping goes deeper from
-- left to right
vectorOf :: Int -> ScopedGen env a -> ScopedGen env [a]
vectorOf n gen = go n
  where
    go 0 = return []
    go k = do
      v <- gen
      vs <- scoped (go (k-1))
      return (v : vs)

listOf :: ScopedGen env a -> ScopedGen env [a]
listOf gen = do
  d <- getMaxDepth
  k <- choose (0, d)
  vectorOf k gen

listOf1 :: ScopedGen env a -> ScopedGen env [a]
listOf1 gen = do
  d <- getMaxDepth
  k <- choose (1, max 1 d)
  vectorOf k gen

----------------------------------------
-- | Random choice combinators with failure recovery

-- | Force the generator to backtrack on a diferent choice
genFail :: ScopedGen env a
genFail = empty

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

-- | Picks a generator from a list of labeled generators.
-- Generation frequencies can be later assigned from outside.
-- In case of fail, retries recursively among the rest.
labeledFrequency :: [Labeled Label (ScopedGen env a)] -> ScopedGen env a
labeledFrequency xs = do
  let mkTuple lg = do
        f <- freqOf (getLabel lg)
        return (f, unlabel lg)
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

-- | Run the scoped generator with an initial context
-- This version discards the generated labels map,
-- and raises an exception if the generation process fails
-- due to an empty initial environment. 
-- It is safe in most cases, but be careful!
runScopedGen' :: env -> FreqMap -> MaxDepth
             -> ScopedGen env a -> Gen a
runScopedGen' env freqs depth g = do
  ma <- runScopedGen env freqs depth g
  case ma of 
    Just (Scoped val, _) ->
      return val
    Nothing -> 
      error $ "generation process failed!\n" 
           <> "backtrack failed when using the provided environment"

----------------------------------------
-- | Generation patterns

buildGenWith :: Functor f
             => ([f (ScopedGen env a)] -> ScopedGen env a)
             -> [f (ScopedGen env a)]
             -> [f (ScopedGen env a)]
             -> ScopedGen env a
buildGenWith method terms recs = gen
  where
    gen = do
      limit <- depthLimit
      if limit
        then method terms
        else method (terms <> recs)

buildGenWith' :: Functor f
             => ([f (ScopedGen env a)] -> ScopedGen env a)
             -> [f (ScopedGen env a)]
             -> [f (ScopedGen env a -> ScopedGen env a)]
             -> ScopedGen env a
buildGenWith' method terms recs = gen
  where
    gen = do
      limit <- depthLimit
      if limit
        then method terms
        else method (terms <> (fmap . fmap) ($ smaller gen) recs)
