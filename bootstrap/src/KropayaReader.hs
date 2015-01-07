{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
module KropayaReader(code, number) where

import Text.Peggy
import Numeric
import Data.Text
import Data.Text.Read
import Data.Maybe

import KropayaTypes

[peggy|

ws :: [Char]
  = ' '+ { "" }

existential_block :: QuantifierBlock
  = '∃' (identifier ws { Variable $1 })+ '.' { ExistentialBlock $1 }

universal_block :: QuantifierBlock
  = '∀' (identifier ws { Variable $1 })+ '.' { UniversalBlock $1 }

lambda_block :: QuantifierBlock
  = 'λ' (identifier ws { Variable $1 })+ '.' { LambdaBlock $1 }

quantifier_blocks :: [QuantifierBlock]
  = (existential_block / universal_block / lambda_block)*

code :: [Code] = (txt { KR0Type $ KAtomic $1 } / number { KR0Type $ KAtomic $1 } / nl) nl? { $1 : (maybeToList $2) }

identifier :: Text
  = ([_+]+[_+:]* { $1 ++ $2})? [a-zA-Z] [a-zA-Z0-9_:$!?%=<>-]* { pack ((fromMaybe "" $1) ++ ($2:$3)) } /
  [~!@$%^&*_=\'`/?×÷≠→←⇒⇐⧺⧻§∘≢∨∪∩□∀⊃∈+<>-]+ [:~!@$%^&*_=\'`/?×÷≠→←⇒⇐⧺⧻§∘≢∨∪∩□∀⊃∈+<>-]* { pack $ $1 ++ $2 } /
  '[' ']' { pack "[]" } / '{' '}' { pack "\123\125" } / '…' { pack "…" }

sstring_escapes :: Char
  = ('\\' 'n'  { '\n' })
  / ('\\' 'r'  { '\r' })
  / ('\\' ']'  { ']'  })
  / ('\\' '\\' { '\\' })

qstring_escapes :: Char
  = ('\\' 'n'  { '\n' })
  / ('\\' 'r'  { '\r' })
  / ('\\' '\"' { '"'  })
  / ('\\' '#'  { '#'  })
  / ('\\' '\\' { '\\' })

txt :: Atomic
  = sstring / qstring

nl :: Code
  = ('\n' '\r' / '\n' / '.') { NL }

sstring :: Atomic
  = '#' '[' ([^\]\\] / sstring_escapes)* ']' { KSSt $ pack $1 }

qstring :: Atomic
  = ('\"' (
      ('#' '{' code? '}' { maybe (Right $ pack "") (\x -> Left x) $1 }) /
      ((qstring_escapes / [^#\"\\])+ { Right $ pack $1 })
    )* '\"') { KQSt $1 } --"

number :: Atomic
  = kdecimal
  / hinteger
  / dinteger

sign :: Maybe Char
  = [-+]?

kdecimal :: Atomic
  = sign [0-9]+ '.' [0-9]+ { KDec (sigrat $1 ($2 ++ "." ++ $3)) }

dinteger :: Atomic
  = sign [0-9]+ { KInt (sigdec $1 $2) }

hinteger :: Atomic
  = sign '0' 'x' [0-9a-fA-F]+ { KInt (sighex $1 $2) }
|]

sighex = sigthing hexadecimal
sigdec = sigthing decimal
sigrat = sigthing rational

sigthing :: Num n => Reader n -> Maybe Char -> String -> n
sigthing thing (Just sign) mag = case signed thing (pack (sign : mag)) of Right (number, _) -> number
                                                                          _                 -> 0 
sigthing thing _ mag = case thing (pack mag) of Right (number, _) -> number
                                                _                 -> 0 

