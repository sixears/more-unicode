{-# LANGUAGE UnicodeSyntax #-}

module Data.MoreUnicode.Semigroup
  ( (◇) )
where

-- base --------------------------------

import Data.Semigroup  ( Semigroup, (<>) )

--------------------------------------------------------------------------------

(◇) ∷ Semigroup α ⇒ α → α → α
(◇) = (<>)

-- that's all, folks! ----------------------------------------------------------
