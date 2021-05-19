{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnicodeSyntax   #-}

module Data.MoreUnicode.Bool
  ( 𝔹, pattern 𝕱, pattern 𝕿 )
where

-- base --------------------------------

import Data.Bool  ( Bool( True, False ) )

--------------------------------------------------------------------------------

type 𝔹 = Bool

pattern 𝕱 ∷ 𝔹
pattern 𝕱 = False
pattern 𝕿 ∷ 𝔹
pattern 𝕿 = True
{-# COMPLETE 𝕿, 𝕱 #-}

-- that's all, folks! ----------------------------------------------------------
