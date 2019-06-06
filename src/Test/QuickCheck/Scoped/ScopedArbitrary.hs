{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.QuickCheck.Scoped.ScopedArbitrary where

import Test.QuickCheck.Scoped.ScopedGen

class ScopedArbitrary a where

  -- | An associated environment for each `a`
  type Env a :: *

  -- | Scoped generation within a context
  scopedArbitrary :: ScopedGen (Env a) a

  -- | Scoped shrinking (i.e. every subterm is well-scoped)
  -- scopedShrink :: ScopedShrinker (Env a) a
