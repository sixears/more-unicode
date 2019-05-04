{-# LANGUAGE UnicodeSyntax #-}

module Data.MoreUnicode.Function
  ( (⅋) )
where

import Prelude ()

-- base --------------------------------

import Data.Function   ( (&) )

-------------------------------------------------------------------------------

infixl 3 ⅋
{-| a version of `(&)` (reverse-application) with higher precedence (3) to allow
    it to more easily be used with, e.g., `(≟)` which has a precedence of 1
-}
(⅋) ∷ α → (α → β) → β
(⅋) = (&)

-- that's all, folks! ---------------------------------------------------------
