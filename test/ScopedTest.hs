{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module ScopedTest where

import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck.Scoped


data T = A Int | B deriving Show

genT :: ScopedGen [Int] (T,T)
genT = do
  t1 <- oneOf [A <$> fromEnv elements, pure B]
  t2 <- oneOf [A <$> fromEnv elements, A <$> fromEnv elements]
  return (t1,t2)


-- | A shitty imperative like language with variable definition, access and
-- deletion. Scoping should be preserved among successive elements of any block.

data Inst v
  = Def v
  | Get v
  | Undef v
  | Block [Inst v]

instance Show v => Show (Inst v) where
  show = ppShow 0
    where
      ppShow n (Def v) = replicate n ' ' ++ "def " ++ show v
      ppShow n (Get v) = replicate n ' ' ++ "get " ++ show v
      ppShow n (Undef v) = replicate n ' ' ++ "undef " ++ show v
      ppShow n (Block ins) = replicate n ' ' ++ "block:"
        ++ concatMap (\i -> "\n" ++ replicate n ' ' ++ ppShow (n+2) i) ins


genInst :: ScopedGen (Set Char) (Inst Char)
genInst = buildGenWith labeledFrequency
  [ #def :=
      Def <$> elements ['a'..'z'] |- Set.insert
  , #get :=
      Get <$> fromEnv elements
  , #undef :=
      Undef <$> fromEnv elements |- Set.delete
  ]
  [ #block :=
      Block <$> scoped (listOf1 (smaller genInst))
  ]

instFreq :: FreqMap
instFreq = freqMap
  [ #def   := const 1
  , #get   := const 1
  , #block := id
  ]

instance ScopedArbitrary (Inst Char) where
  type Env (Inst Char) = Set Char
  scopedArbitrary = genInst

----------------------------------------

-- | Closed lambda terms.
-- Leftmost expression is always a lambda

data Expr
  = Var Char
  | Expr `App` Expr
  | Lam Char Expr

instance Show Expr where
  show (Var v) = [v]
  show (App l r) = "(" ++ show l ++ ")(" ++ show r ++")"
  show (Lam v e) = "\\" ++ [v] ++ ". " ++ show e

genExpr :: ScopedGen (Set Char) Expr
genExpr = buildGenWith labeledFrequency
  [
    #var :=
      Var <$> fromEnv elements
  ]
  [
    #app :=
      App <$> scoped (smaller genExpr)
          <*> scoped (smaller genExpr)
  , #lam :=
      Lam <$> elements ['a' .. 'z'] |- Set.insert
          <*> scoped (smaller genExpr)
  ]
