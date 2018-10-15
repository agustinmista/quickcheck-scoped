{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.QuickCheck.Scoped.Arbitrary where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Scoped.Gen

class ScopedArbitrary a where
  -- | An associated context for each `a`
  type Context a :: *
  -- | Scoped generation within a context
  scopedArbitrary :: ScopedGen (Context a) a
  -- -- | Scoped shrinking (i.e. every subterm is well-scoped)
  -- scopedShrink :: a -> [a]

instance {-# OVERLAPPING #-} (ScopedArbitrary a, Monoid (Context a))
                          => Arbitrary (Maybe (Scoped a)) where
  arbitrary = runEmptyScopedGen scopedArbitrary
