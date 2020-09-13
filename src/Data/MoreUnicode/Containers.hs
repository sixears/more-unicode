{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.MoreUnicode.Containers
  ( (∈) )
where

-- base --------------------------------

import Data.Ord  ( Ord )

-- containers --------------------------

import qualified  Data.Map  as  Map

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Data.MoreUnicode.Bool  ( 𝔹 )

--------------------------------------------------------------------------------

(∈) ∷ ∀ κ α . Ord κ ⇒ κ → Map.Map κ α → 𝔹
(∈) = Map.member

-- that's all, folks! ----------------------------------------------------------
