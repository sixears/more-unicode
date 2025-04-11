{-# LANGUAGE UnicodeSyntax #-}
module Data.MoreUnicode.Maybe
  ( 𝕄
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
pattern 𝕵 ∷ α → 𝕄 α
pattern 𝕵 a ← Just a
        where 𝕵 a = Just a
{-| Unicode alias for `Nothing` -}
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
