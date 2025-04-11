{-# LANGUAGE UnicodeSyntax #-}
module Data.MoreUnicode.Ord
  ( (≶)
  , (≷)
  ) where

-- base --------------------------------

import Data.Ord ( Ord((<), (>)), Ordering(EQ, GT, LT), compare )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Data.MoreUnicode.Maybe ( 𝕄, (⧐) )

--------------------------------------------------------------------------------

{-| Unicode alias for `Data.Ord.compare` -}
infix 4 ≷
(≷) ∷ Ord α ⇒ α → α → Ordering
(≷) = compare

{-| "is within bounds"; if a is within (l,u) inclusive, then EQ; else LT or GT
    as appropriate -}
(≶) ∷ Ord α ⇒ α → (𝕄 α, 𝕄 α) → Ordering
a ≶ (l,u) =
  let ḻ = a ⧐ l -- lower bound, use a if none provided
      ū = a ⧐ u -- upper bound, use a if none provided
  in if a < ḻ then LT else if a > ū then GT else EQ

-- that's all, folks! ----------------------------------------------------------
