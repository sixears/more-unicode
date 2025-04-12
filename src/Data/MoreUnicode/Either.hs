{-# LANGUAGE UnicodeSyntax #-}
module Data.MoreUnicode.Either
  ( 𝔼
  , pattern 𝓛
  , pattern 𝓡
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

{-| Unicode alias for `Either` -}
type 𝔼 = Either

{-| Unicode alias for `Left` -}
pattern 𝓛 ∷ α → 𝔼 α β
pattern 𝓛 a ← Left a
        where 𝓛 a = Left a

{-| Unicode alias for `Right` -}
pattern 𝓡 ∷ β → 𝔼 α β
pattern 𝓡 b ← Right b
        where 𝓡 b = Right b

{-# COMPLETE 𝓛, 𝓡 #-}

{-| Unicode alias for `Left` -}
{-# DEPRECATED 𝕷 "use '𝓛' instead" #-}
pattern 𝕷 ∷ α → 𝔼 α β
pattern 𝕷 a ← Left a
        where 𝕷 a = Left a

{-| Unicode alias for `Right` -}
{-# DEPRECATED 𝕽 "use '𝓡' instead" #-}
pattern 𝕽 ∷ β → 𝔼 α β
pattern 𝕽 b ← Right b
        where 𝕽 b = Right b

{-# COMPLETE 𝕷, 𝕽 #-}

{-| used for errors; converts 𝓛 to a `raise` (e.g., use on MonadError) -}
ӿ ∷ Printable ε ⇒ 𝔼 ε α → α
ӿ = \ case 𝓛 e → error (toString e); 𝓡 r → r

-- that's all, folks! ----------------------------------------------------------
