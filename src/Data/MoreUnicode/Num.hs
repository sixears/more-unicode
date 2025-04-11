{-# LANGUAGE UnicodeSyntax #-}
module Data.MoreUnicode.Num
  ( (÷)
  ) where

-- base --------------------------------

import Data.Ratio ( Ratio, (%) )
import GHC.Real   ( Integral )

--------------------------------------------------------------------------------

{-| Construct a Rational number, with (e.g.,) 2÷3 -}
(÷) ∷ Integral α ⇒ α → α → Ratio α
(÷) = (%)

-- that's all, folks! ----------------------------------------------------------
