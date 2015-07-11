{-# LANGUAGE OverloadedStrings #-}

import Telegram

import qualified Data.ByteString.Lazy.Char8 as L

import Control.Monad (mapM_)

import Data.List (isInfixOf)
import Data.Char (toLower)

--[ ("who.*fat",      "I'm fat!")
--, ("harrison.*fat", "No you")
--, ("harrison is",   "No you")
--, ("rock",          "RRROOCCKKSS!") ]

main :: IO ()
main = do
  loopForUpdates 0

