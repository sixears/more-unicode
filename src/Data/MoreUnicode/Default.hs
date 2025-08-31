{-| Unicode aliases for the Default module -}
module Data.MoreUnicode.Default
  ( Ð, ð
  ) where

-- data-default ------------------------

import Data.Default  ( Default( def ) )

--------------------------------------------------------------------------------

type Ð = Default
ð ∷ Ð α ⇒ α
ð = def

-- that's all, folks! ----------------------------------------------------------
