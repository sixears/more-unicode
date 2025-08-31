{-# LANGUAGE UnicodeSyntax #-}
module Data.MoreUnicode.Monad
  ( 𝕀, pattern 𝕀
  , (≪)
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
  , (⮆), (⮄)
  , 𝕣
  ) where

-- base --------------------------------

import Control.Monad    ( Monad, forM, forM_, mapM, mapM_, (<=<), (=<<), (>=>),
                          (>>), (>>=) )
import Data.Foldable    ( Foldable )
import Data.Function    ( flip )
import Data.Functor     ( Functor, fmap )
import Data.Traversable ( Traversable )

-- mtl ---------------------------------

import Control.Monad.Identity  ( Identity( Identity ), IdentityT, runIdentity
                               , runIdentityT )
import Control.Monad.Reader    ( ReaderT, runReaderT )
import Control.Monad.State     ( StateT, runStateT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Data.MoreUnicode.Default  ( Ð, ð )

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

--------------------------------------------------------------------------------

{-| Unicode alias for the Identity Monad -}
type 𝕀 = Identity

{-| Unicode alias for the Identity Monad data c'tor -}
pattern 𝕀 ∷ α → 𝕀 α
pattern 𝕀 a ← Identity a
  where 𝕀 a = Identity a
{-# COMPLETE 𝕀 #-}

------------------------------------------------------------

class RunnableMonad η where
  type MonadRunType    η
  type MonadResultType η

  (⮆) ∷ MonadRunType η → η → MonadResultType η
  (⮄) ∷ η → MonadRunType η → MonadResultType η
  (⮄) = flip (⮆)
  𝕣   ∷ Ð (MonadRunType η) => η → MonadResultType η
  𝕣   = (ð ⮆)

--------------------

instance RunnableMonad (𝕀 ω) where
  type MonadRunType (𝕀 ω) = ()
  type MonadResultType (𝕀 ω) = ω

  () ⮆ ia = runIdentity ia

--------------------

instance RunnableMonad (IdentityT ζ ω) where
  type MonadRunType (IdentityT ζ ω) = ()
  type MonadResultType (IdentityT ζ ω) = ζ ω

  () ⮆ ia = runIdentityT ia

--------------------

instance RunnableMonad (ReaderT ρ ζ ω) where
  type MonadRunType (ReaderT ρ ζ ω) = ρ
  type MonadResultType (ReaderT ρ ζ ω) = ζ ω

  r ⮆ m = runReaderT m r

--------------------

instance RunnableMonad (StateT σ ζ ω) where
  type MonadRunType (StateT σ ζ ω) = σ
  type MonadResultType (StateT σ ζ ω) = ζ (ω, σ)

  s ⮆ m = runStateT m s

-- that's all, folks! ----------------------------------------------------------
