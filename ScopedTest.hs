{-# LANGUAGE TypeFamilies #-}
module ScopedTest where

import Data.Set (Set)
import qualified Data.Set as Set

import Test.QuickCheck.Scoped.Gen
import Test.QuickCheck.Scoped.Arbitrary


data T = A Int | B deriving Show

genT :: ScopedGen [Int] (T,T)
genT = do
  t1 <- oneOf [A <$> fromContext, pure B]
  t2 <- oneOf [A <$> fromContext, A <$> fromContext]
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

genInst :: (Arbitrary v, Ord v) => ScopedGen (Set v) (Inst v)
genInst = sizedFreqsGen [(1,def), (2,get), (1,undef)] [(2,block)]
  where
    -- | How do we generate each instruction
    def = Def <$> liftArbitrary
    get = Get <$> fromContext
    undef = Undef <$> fromContext
    block gen = Block <$> listOf1 updateCtx (gen (./5))

    -- | How to update the context when we generate each instruction
    updateCtx (Def v)   = Set.insert v
    updateCtx (Undef v) = Set.delete v
    updateCtx _         = id

instance (Arbitrary v, Ord v) => ScopedArbitrary (Inst v) where
  type Context (Inst v) = Set v
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
  show (App l r) = "(" ++ show l ++ ") (" ++ show r ++")"
  show (Lam v e) = "\\" ++ [v] ++ ". " ++ show e

genExpr :: ScopedGen (Set Char) Expr
genExpr = sizedUniformGen [var] [app, lam]
  where
    var = Var <$> fromContext
    app gen = App <$> gen (./2) <*> gen (./2)
    lam gen = do v <- elements ['a' .. 'z']
                 e <- scoped (Set.insert v) (gen ((-) 1))
                 return (Lam v e)

(./) = flip div
