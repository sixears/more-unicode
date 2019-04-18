{-# LANGUAGE UnicodeSyntax #-}

module Data.MoreUnicode.Lens
  ( (⊣), (⊢), (⩼), (##) )
where

-- base --------------------------------

import Data.Function  ( flip )
import Data.Maybe     ( Maybe )
import Data.Monoid    ( First )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Getter  ( Getting, (^.) )
import Control.Lens.Review  ( AReview, (#) )
import Control.Lens.Setter  ( ASetter, (.~) )

--------------------------------------------------------------------------------

infixl 8 ⊣
(⊣) ∷ δ → Getting α δ α → α
(⊣) = (^.)

infixr 4 ⊢
(⊢) ∷ ASetter σ τ α β → β → σ → τ
(⊢) = (.~)

infixl 8 ⩼
(⩼) ∷ σ → Getting (First α) σ α → Maybe α
(⩼) = (^?)

infixr 8 ##
(##) ∷ α → AReview β α → β
(##) = flip (#)

-- that's all, folks! ----------------------------------------------------------
