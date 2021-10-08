module Data.MoreUnicode.MonoTraversable
  ( (⪧), (⪦) )
where

import Prelude ()

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFunctor( omap ) )

-------------------------------------------------------------------------------

infixl 4 ⪧
{- | alias for `omap`, that is, `Data.Functor.fmap` over a monofunctor -}
(⪧) ∷ MonoFunctor ψ ⇒ (Element ψ → Element ψ) → ψ → ψ
(⪧) = omap

-- | flip `(⪧)`
{- | flipped alias for `omap`, that is, `Data.Functor.fmap` over a monofunctor
 -}
infixl 4 ⪦
(⪦) ∷ MonoFunctor ψ ⇒ ψ → (Element ψ → Element ψ) → ψ
as ⪦ f = omap f as

-- that's all, folks! ---------------------------------------------------------
