module Data.MoreUnicode.Either
  ( 𝔼, pattern 𝕷, pattern 𝕽 )
where

-- base --------------------------------

import Data.Either  ( Either( Left, Right ) )

--------------------------------------------------------------------------------

type 𝔼 = Either

pattern 𝕷 ∷ α → 𝔼 α β
pattern 𝕷 a ← Left a
        where 𝕷 a = Left a

pattern 𝕽 ∷ β → 𝔼 α β
pattern 𝕽 b ← Right b
        where 𝕽 b = Right b

{-# COMPLETE 𝕷, 𝕽 #-}

-- that's all, folks! ----------------------------------------------------------
