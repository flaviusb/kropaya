{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
module Main where

import Text.Peggy
import Numeric
import Data.Text
import Data.Text.Read
import Data.Maybe

import KropayaTypes

[peggy|
top :: Integer = expr !.

expr :: Integer
  = expr "+" expr { $1 + $2 }
  / expr "-" expr { $1 - $2 }
  / number

number :: Integer
  = hinteger
  / dinteger

sign :: Maybe Char
  = [-+]?

dinteger :: Integer
  = sign [0-9]+ { sigdec $1 $2 }

hinteger :: Integer
  = sign "0x" [0-9a-fA-F]+ { sighex $1 $2 }
|]

sighex = sigthing hexadecimal
sigdec = sigthing decimal

sigthing :: Reader Integer -> Maybe Char -> String -> Integer
sigthing thing (Just sign) mag = case signed thing (pack (sign : mag)) of Right (number, _) -> number
                                                                          _                 -> 0 
sigthing thing _ mag = case thing (pack mag) of Right (number, _) -> number
                                                _                 -> 0 

main :: IO ()
main =
  print . parseString dinteger "" =<< getContents
