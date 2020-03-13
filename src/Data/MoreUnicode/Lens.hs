{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax    #-}

module Data.MoreUnicode.Lens
  ( (⊣), (⫣), (⫥), (⊥), (⊢), (⊧), (⊩), (⩼), (⊮), (##), (⋖), (⋗)
  , addMaybe, tindex )
where

import Prelude  ( Int, fromIntegral )

-- base --------------------------------

import Control.Applicative  ( Applicative )
import Data.Function        ( flip )
import Data.Maybe           ( Maybe )
import Data.Monoid          ( First )
import Data.Traversable     ( Traversable )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- lens --------------------------------

import Control.Lens.Cons       ( Cons, Snoc, (|>), (<|) )
import Control.Lens.Fold       ( (^?) )
import Control.Lens.Getter     ( Getting, (^.) )
import Control.Lens.Indexed    ( Indexable, index )
import Control.Lens.Iso        ( AnIso, from )
import Control.Lens.Review     ( AReview, (#) )
import Control.Lens.Setter     ( ASetter, (.~), (%~), (?~) )
import Control.Lens.Traversal  ( traversed )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Data.MoreUnicode.Applicative  ( (∤) )
import Data.MoreUnicode.Natural      ( ℕ )

--------------------------------------------------------------------------------

{- | Unicode alias for `(^.)` -}
infixl 8 ⊣
(⊣) ∷ δ → Getting α δ α → α
(⊣) = (^.)

{- | Reversal of `(⊣)` for isos; that is, equivalent to `x ^. from l` -}
infixl 8 ⫣
(⫣) ∷ δ → AnIso α α δ δ → α
d ⫣ l = d ^. from l

{- | Getter for prisms; use `object ⫥ prism` to get the value. -}
infixr 8 ⫥
(⫥) ∷ α → AReview δ α → δ
(⫥) = flip (#)

infixr 4 ⊢
(⊢) ∷ ASetter σ τ α β → β → σ → τ
(⊢) = (.~)

infixr 4 ⊧
(⊧) ∷ ASetter σ τ α β → (α → β) → σ → τ
(⊧) = (%~)

{- | Alias of `(?~)`.  Set the target of a Lens, Traversal or Setter to Just a
     value. -}

infixr 4 ⊩
(⊩) ∷ ASetter σ τ α (Maybe β) → β → σ → τ
(⊩) = (?~)

infixl 8 ⩼
(⩼) ∷ σ → Getting (First α) σ α → Maybe α
(⩼) = (^?)

addMaybe ∷ ASetter σ τ (Maybe α) (Maybe α) → Maybe α → σ → τ
addMaybe s a = s ⊧ (∤ a)

infixr 4 ⊮
(⊮) ∷ ASetter σ τ (Maybe α) (Maybe α) → Maybe α → σ → τ
(⊮) = addMaybe

{- | DEPRECATED (##) "use `⫥` instead" -}
infixr 8 ##
(##) ∷ α → AReview δ α → δ
(##) = flip (#)

{-| index into a traversable thing) -}
tindex ∷ (Indexable Int ι, Traversable ψ, Applicative ξ) =>
         ((ψ α -> ξ (ψ α)) -> β) -> ℕ -> ι α (ξ α) -> β

tindex l i = l ∘ traversed ∘ index (fromIntegral i)

infixr 8 ⊥
(⊥) ∷ (Indexable Int ι, Traversable ψ, Applicative ξ) =>
      ((ψ α -> ξ (ψ α)) -> β) -> ℕ -> ι α (ξ α) -> β
(⊥) = tindex

----------------------------------------

(⋖) ∷ Cons σ σ α α => α -> σ -> σ 
(⋖) = (<|)
(⋗) ∷ Snoc σ σ α α => σ -> α -> σ 
(⋗) = (|>)

-- that's all, folks! ----------------------------------------------------------
