{-# LANGUAGE UnicodeSyntax #-}
module Data.MoreUnicode.Monad
  ( (≪)
  , (≫)
  , (⋘)
  , (⋙)
  , (⩤)
  , (⩥)
  , (⪻)
  , (⪼)
  , (⮘)
  , (⮚)
  , (⮜)
  , (⮞)
  ) where

-- base --------------------------------

import Control.Monad    ( Monad, forM, forM_, mapM, mapM_, (<=<), (=<<), (>=>),
                          (>>), (>>=) )
import Data.Foldable    ( Foldable )
import Data.Function    ( flip )
import Data.Functor     ( Functor, fmap )
import Data.Traversable ( Traversable )

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

-- | unicode alias for `(>=>)`
infixr 1 ⋙
(⋙) ∷ Monad η ⇒ (α → η β) → (β → η γ) → α → η γ
(⋙) = (>=>)

-- | unicode alias for `(<=<)`
infixr 1 ⋘
(⋘) ∷ Monad η ⇒ (β → η γ) → (α → η β) → α → η γ
(⋘) = (<=<)

{- | A bit like `(=<<)` / `(⊲)`, but allows the lhs to be a function itself
     for point-free styling.
-}
(⩤) ∷ (Monad η, Functor ψ) ⇒ (α → η β) → ψ (η α) → ψ (η β)
x ⩤ y = fmap (x ≪) y

{- | A bit like `(>>=)` / `(⊳)`, but allows the rhs to be a function itself
     for point-free styling.
-}
(⩥) ∷ (Monad η, Functor ψ) ⇒ ψ (η α) → (α → η β) → ψ (η β)
x ⩥ y = fmap (≫ y) x

{-| unicode alias for `mapM` -}
(⮞) ∷ (Monad η, Traversable ψ) ⇒ (α → η β) → ψ α → η (ψ β)
(⮞) = mapM

{-| unicode alias for `mapM_` -}
(⮚) ∷ (Monad η, Foldable φ) ⇒ (α → η ()) → φ α → η ()
(⮚) = mapM_

{-| unicode alias for `forM` -}
(⮜) ∷ (Monad η, Traversable ψ) ⇒ ψ α → (α → η β) → η (ψ β)
(⮜) = forM

{-| unicode alias for `forM_` -}
(⮘) ∷ (Monad η, Foldable φ) ⇒ φ α → (α → η ()) → η ()
(⮘) = forM_

-- that's all, folks! ----------------------------------------------------------
