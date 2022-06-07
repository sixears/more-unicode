module Data.MoreUnicode.Bool
  ( 𝔹, pattern 𝕱, pattern 𝕿, ﬧ )
where

-- base --------------------------------

import Data.Bool  ( Bool( True, False ), not )

--------------------------------------------------------------------------------

type 𝔹 = Bool

pattern 𝕱 ∷ 𝔹
pattern 𝕱 = False
pattern 𝕿 ∷ 𝔹
pattern 𝕿 = True
{-# COMPLETE 𝕿, 𝕱 #-}

-- that's actually a Hebrew "wide resh"
ﬧ ∷ 𝔹 → 𝔹
ﬧ = not


-- that's all, folks! ----------------------------------------------------------
