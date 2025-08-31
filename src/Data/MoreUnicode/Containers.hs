{-# LANGUAGE UnicodeSyntax #-}
module Data.MoreUnicode.Containers
  ( Member((∈), (∉))
  ) where

-- base --------------------------------

import Data.Foldable qualified as Foldable

import Data.Eq       ( Eq )
import Data.Foldable ( Foldable )
import Data.Kind     ( Type )
import Data.Word     ( Word8 )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- bytestring --------------------------

import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BSL

-- text --------------------------------

import Data.Text      qualified as Text
import Data.Text.Lazy qualified as LazyText

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Data.MoreUnicode.Bool ( 𝔹, ﬧ )
import Data.MoreUnicode.Char ( ℂ )
import Data.MoreUnicode.Text ( 𝕋 )

--------------------------------------------------------------------------------

class Member α where
  {-| What do items of this container type look like? -}
  type MemberItem α ∷ Type
  {-| "is element of" -}
  (∈) ∷ Eq (MemberItem α) ⇒ MemberItem α → α → 𝔹
  {-| "is not element of" -}
  (∉) ∷ Eq (MemberItem α) ⇒ MemberItem α → α → 𝔹
  (∉) = \ x → ﬧ ∘ (x ∈)

instance Foldable ψ ⇒ Member (ψ β) where
  type MemberItem (ψ β) = β
  (∈) = Foldable.elem

instance Member 𝕋 where
  type MemberItem 𝕋 = ℂ
  (∈) = Text.elem

instance Member LazyText.Text where
  type MemberItem (LazyText.Text) = ℂ
  (∈) = LazyText.elem

instance Member BS.ByteString where
  type MemberItem BS.ByteString = Word8
  (∈) = BS.elem

instance Member BSL.ByteString where
  type MemberItem BSL.ByteString = Word8
  (∈) = BSL.elem

-- that's all, folks! ----------------------------------------------------------
