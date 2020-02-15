{-# LANGUAGE UnicodeSyntax #-}

module Data.MoreUnicode.Tasty
  ( (≟), (≣) )
where

-- base --------------------------------

import Data.Eq    ( Eq )
import Text.Show  ( Show )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, (@=?) )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Property, (===) )

--------------------------------------------------------------------------------

{- | Pronounced 'test', this tests for equality; it's `(@=?)`; note that puts
     the 'got' or 'actual' value as the last argument, to allow for easier
     partial application.
 -}
{-# DEPRECATED (≟) "use `TastyPlus.(≟)` or (@=?) instead" #-}
infix 1 ≟
(≟) ∷ (Eq α, Show α) ⇒ α → α → Assertion
(≟) = (@=?)
  
----------------------------------------

{- | synonym for `===` -}
infix 4 ≣
(≣) ∷ (Eq a, Show a) ⇒ a → a → Property
(≣) = (===)

-- that's all, folks! ----------------------------------------------------------
