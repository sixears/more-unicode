{-# LANGUAGE UnicodeSyntax #-}
module Data.MoreUnicode.Maybe
  ( 𝕄
  , pattern 𝓙
  , pattern 𝓝
  , pattern 𝕵
  , pattern 𝕹
  , ⅎ
  , (⧏)
  , (⧐)
  ) where

{-| Unicode forms for `Data.Maybe` -}

-- base --------------------------------

import Data.Function ( flip )
import Data.Maybe    ( Maybe(Just, Nothing), fromJust, fromMaybe )

--------------------------------------------------------------------------------

{-| Unicode alias for `Maybe` -}
type 𝕄 = Maybe

{-| Unicode alias for `Just` -}
pattern 𝓙 ∷ α → 𝕄 α
pattern 𝓙 a ← Just a
        where 𝓙 a = Just a
{-| Unicode alias for `Nothing` -}
pattern 𝓝 ∷ 𝕄 α
pattern 𝓝 = Nothing
{-# COMPLETE 𝓙, 𝓝 #-}

{-| Unicode alias for `Just` -}
{-# DEPRECATED 𝕵 "use '𝓙' instead" #-}
pattern 𝕵 ∷ α → 𝕄 α
pattern 𝕵 a ← Just a
        where 𝕵 a = Just a
{-| Unicode alias for `Nothing` -}
{-# DEPRECATED 𝕹 "use '𝓝' instead" #-}
pattern 𝕹 ∷ 𝕄 α
pattern 𝕹 = Nothing
{-# COMPLETE 𝕵, 𝕹 #-}

infixr 3 ⧏
{-| Unicode alias for `flip fromMaybe` -}
(⧏) ∷ 𝕄 α → α → α
(⧏) = flip fromMaybe

infixl 3 ⧐
{-| Unicode alias for `fromMaybe` -}
(⧐) ∷ α → 𝕄 α → α
(⧐) = fromMaybe

{-| Unicode alias for `fromJust` -}
ⅎ ∷ 𝕄 α → α
ⅎ = fromJust

-- that's all, folks! ----------------------------------------------------------
