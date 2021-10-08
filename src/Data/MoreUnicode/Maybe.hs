module Data.MoreUnicode.Maybe
  ( 𝕄, pattern 𝕵, pattern 𝕹 )
where

-- base --------------------------------

import Data.Maybe  ( Maybe( Just, Nothing ) )

--------------------------------------------------------------------------------

type 𝕄 = Maybe

pattern 𝕵 ∷ α → 𝕄 α
pattern 𝕵 a ← Just a
        where 𝕵 a = Just a
pattern 𝕹 ∷ 𝕄 α
pattern 𝕹 = Nothing
{-# COMPLETE 𝕵, 𝕹 #-}

-- that's all, folks! ----------------------------------------------------------
