{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
module Main where

import Text.Peggy
import Numeric
import Data.Text
import Data.Text.Read
import Data.Maybe

import KropayaTypes
import KropayaReader


main :: IO ()
main =
  print . parseString number "" =<< getContents
