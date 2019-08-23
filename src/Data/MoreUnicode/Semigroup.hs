{-# LANGUAGE UnicodeSyntax #-}

module Data.MoreUnicode.Semigroup
  ( (◇) )
where

-- base --------------------------------

import Data.Semigroup  ( Semigroup, (<>) )

--------------------------------------------------------------------------------

{- | alias for `(<>)`, that is, `Data.Monoid.mappend` for semigroups -}
(◇) ∷ Semigroup α ⇒ α → α → α
(◇) = (<>)

-- that's all, folks! ----------------------------------------------------------
