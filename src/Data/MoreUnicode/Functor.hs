{-# LANGUAGE UnicodeSyntax #-}

module Data.MoreUnicode.Functor
  ( (⊳), (⊲) )
where

import Prelude ()

-- base --------------------------------

import Data.Functor   ( Functor, fmap )

-------------------------------------------------------------------------------

infixl 4 ⊳
(⊳) ∷ Functor ψ ⇒ (α → β) → ψ α → ψ β
(⊳) = fmap

-- | Like `(⊳)`, but with the arguments flipped.  This might be useful, for
--   example as an infix version of `liftM`.  Designed to have similar flow to
--   `(Control.Monad.>>=)`.
--   Thus,
--     readFile "/etc/passwd" >>$ length :: IO Int
infixl 4 ⊲
(⊲) ∷ Functor ψ ⇒ ψ α → (α → β) → ψ β
as ⊲ f = fmap f as

-- that's all, folks! ---------------------------------------------------------
