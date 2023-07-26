module Data.MoreUnicode.Lens
  ( (⊣), (⫣), (⫤), (⫥), (⊥), (⊢), (⊧), (⊩), (⩼), (⊮), (##), (⋖), (⋗), (⨦)
  , (?+), addMaybe, tindex )
where

import Prelude  ( Int, fromIntegral )

-- base --------------------------------

import Control.Applicative  ( Applicative )
import Data.Function        ( flip )
import Data.Maybe           ( fromMaybe )
import Data.Monoid          ( First )
import Data.Traversable     ( Traversable )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- lens --------------------------------

import Control.Lens.At         ( At, Index, IxValue, at )
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
import Data.MoreUnicode.Maybe        ( 𝕄, pattern 𝕵 )
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

infixr 8 ⫤
(⫤) ∷ At δ ⇒ δ → Index δ → 𝕄 (IxValue δ)
x ⫤ y = x ⊣ at y

infixr 4 ⊢
(⊢) ∷ ASetter σ τ α β → β → σ → τ
(⊢) = (.~)

infixr 4 ⊧
(⊧) ∷ ASetter σ τ α β → (α → β) → σ → τ
(⊧) = (%~)

{- | Alias of `(?~)`.  Set the target of a Lens, Traversal or Setter to Just a
     value. -}

infixr 4 ⊩
(⊩) ∷ ASetter σ τ α (𝕄 β) → β → σ → τ
(⊩) = (?~)

infixl 8 ⩼
(⩼) ∷ σ → Getting (First α) σ α → 𝕄 α
(⩼) = (^?)

addMaybe ∷ ASetter σ τ (𝕄 α) (𝕄 α) → 𝕄 α → σ → τ
addMaybe s a = s ⊧ (∤ a)

infixr 4 ⊮
(⊮) ∷ ASetter σ τ (𝕄 α) (𝕄 α) → 𝕄 α → σ → τ
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

----------------------------------------

infixr 4 ?+
{- | Defaulting of a `Maybe` value; that is, assign `Just` a value to the
     target iff it is a `Nothing`. -}
(?+) ∷ ∀ α σ τ . ASetter σ τ (𝕄 α) (𝕄 α) → α → σ → τ
b ?+ y = b ⊧ (𝕵 ∘ fromMaybe y)
infixr 4 ⨦
(⨦) ∷ ∀ α σ τ . ASetter σ τ (𝕄 α) (𝕄 α) → α → σ → τ
(⨦) = (?+)

-- that's all, folks! ----------------------------------------------------------
