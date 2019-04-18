{-# LANGUAGE UnicodeSyntax #-}

module Perg
  ( perg )
where

-- base --------------------------------

import Control.Monad  ( forever, when )
import Data.Function  ( ($) )
import Data.List      ( isInfixOf )
import Data.String    ( String )
import System.IO      ( IO, getLine, putStrLn )

--------------------------------------------------------------------------------

{-| read stdin forever, printing only lines that contain a given string -}
perg ∷ String -> IO ()
perg match = forever $ do
  l <- getLine
  when (match `isInfixOf` l) $ putStrLn l

-- that's all, folks! ----------------------------------------------------------
