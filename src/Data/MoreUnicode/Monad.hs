module Data.MoreUnicode.Monad
  ( (⪻), (⪼), (≪), (≫), (⋘), (⋙), (⩤), (⩥) )
where

-- base --------------------------------

import Control.Monad  ( Monad, (>>), (>>=), (=<<), (<=<), (>=>) )
import Data.Function  ( flip )
import Data.Functor   ( Functor, fmap )

--------------------------------------------------------------------------------

-- | unicode alias for `(>>)`
infixl 1 ⪼
(⪼) ∷ Monad η ⇒ η α → η β → η β
(⪼) = (>>)

-- | flip `(⪼)`
infixl 1 ⪻
(⪻) ∷ Monad η ⇒ η β → η α → η β
(⪻) = flip (⪼)

-- | unicode alias for `(>>=)`
infixl 1 ≫
(≫) ∷ Monad η ⇒ η α → (α → η β) → η β
(≫) = (>>=)

-- | unicode alias for `(=<<)`
infixr 1 ≪
(≪) ∷ Monad η ⇒ (α → η β) → η α → η β
(≪) = (=<<)

-- | unicode alias for `(<=<)`
infixr 1 ⋙
(⋙) ∷ Monad η ⇒ (α → η β) → (β → η γ) → α → η γ
(⋙) = (>=>)

-- | unicode alias for `(<=<)`
infixr 1 ⋘
(⋘) ∷ Monad η ⇒ (β → η γ) → (α → η β) → α → η γ
(⋘) = (<=<)

{- | A bit like `(=<<)` / `(⊲)`, but allows the rhs to be a function itself
     for point-free styling.
-}
(⩤) ∷ (Monad η, Functor ψ) ⇒ (α → η β) → ψ (η α) → ψ (η β)
x ⩤ y = fmap (x ≪) y

{- | A bit like `(>>=)` / `(⊳)`, but allows the rhs to be a function itself
     for point-free styling.
-}
(⩥) ∷ (Monad η, Functor ψ) ⇒ ψ (η α) → (α → η β) → ψ (η β)
x ⩥ y = fmap (≫ y) x

-- that's all, folks! ----------------------------------------------------------
