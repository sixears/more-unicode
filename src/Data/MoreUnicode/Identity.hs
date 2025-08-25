{-| Unicode aliases for the Identity monad -}
module Data.MoreUnicode.Identity
  ( 𝕀, pattern 𝕀
  ) where

-- mtl ---------------------------------

import Control.Monad.Identity  ( Identity( Identity ) )

--------------------------------------------------------------------------------

{-| Unicode alias for the Identity Monad -}
type 𝕀 = Identity

{-| Unicode alias for the Identity Monad data c'tor -}
pattern 𝕀 ∷ α → 𝕀 α
pattern 𝕀 a ← Identity a
  where 𝕀 a = Identity a
{-# COMPLETE 𝕀 #-}

-- that's all, folks! ----------------------------------------------------------
