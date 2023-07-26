module Data.MoreUnicode.Doc
  ( (⊞) )
where

-- Prettyprinter -----------------------

import Prettyprinter  ( Doc, (<+>) )

--------------------------------------------------------------------------------

infixr 5 ⊞
-- hsep
(⊞) ∷ Doc α → Doc α → Doc α
(⊞) = (<+>)

-- that's all, folks! ----------------------------------------------------------
