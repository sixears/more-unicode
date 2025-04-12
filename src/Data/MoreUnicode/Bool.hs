{-# LANGUAGE UnicodeSyntax #-}
module Data.MoreUnicode.Bool
  ( 𝔹
  , pattern 𝓕
  , pattern 𝓣
  , pattern 𝕱
  , pattern 𝕿
  , ﬧ
  ) where

-- base --------------------------------

import Data.Bool ( Bool(False, True), not )

--------------------------------------------------------------------------------

{-| Unicode alias for `Bool` -}
type 𝔹 = Bool

{-| Unicode alias for `False` -}
pattern 𝓕 ∷ 𝔹
pattern 𝓕 = False

{-| Unicode alias for `True` -}
pattern 𝓣 ∷ 𝔹
pattern 𝓣 = True

{-# COMPLETE 𝓣, 𝓕 #-}

{-| Unicode alias for `False` -}
{-# DEPRECATED 𝕱 "use '𝓕' instead" #-}
pattern 𝕱 ∷ 𝔹
pattern 𝕱 = False

{-| Unicode alias for `True` -}
{-# DEPRECATED 𝕿 "use '𝓣' instead" #-}
pattern 𝕿 ∷ 𝔹
pattern 𝕿 = True
{-# COMPLETE 𝕿, 𝕱 #-}

-- that's actually a Hebrew "wide resh"
ﬧ ∷ 𝔹 → 𝔹
ﬧ = not


-- that's all, folks! ----------------------------------------------------------
