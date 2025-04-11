{-# LANGUAGE UnicodeSyntax #-}
module Data.MoreUnicode.Either
  ( 𝔼
  , pattern 𝕷
  , pattern 𝕽
  , ӿ
  ) where

import Prelude ( error )

-- base --------------------------------

import Data.Either ( Either(Left, Right) )

-- data-textual ------------------------

import Data.Textual ( Printable, toString )

--------------------------------------------------------------------------------

type 𝔼 = Either

pattern 𝕷 ∷ α → 𝔼 α β
pattern 𝕷 a ← Left a
        where 𝕷 a = Left a

pattern 𝕽 ∷ β → 𝔼 α β
pattern 𝕽 b ← Right b
        where 𝕽 b = Right b

{-# COMPLETE 𝕷, 𝕽 #-}

{-| used for errors; converts 𝕷 to a `raise` (e.g., use on MonadError) -}
ӿ ∷ Printable ε ⇒ 𝔼 ε α → α
ӿ = \ case 𝕷 e → error (toString e); 𝕽 r → r

-- that's all, folks! ----------------------------------------------------------
