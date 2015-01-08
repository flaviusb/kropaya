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
  = '∃' ws (identifier ws { Variable $1 })+ '.' { ExistentialBlock $2 }

universal_block :: QuantifierBlock
  = '∀' ws (identifier ws { Variable $1 })+ '.' { UniversalBlock $2 }

lambda_block :: QuantifierBlock
  = 'λ' ws (identifier ws { Variable $1 })+ '.' { LambdaBlock $2 }

quantifier_blocks :: [QuantifierBlock]
  = (existential_block / universal_block / lambda_block)*

code :: [Statement] = ((code_value { JustExpression $ Expression [] $ Right $1 }) { [$1] } / nl { [] }) ws* nl? { $1 }

code_value :: CodeValue
 = atomic_value { CVAtomicValue $1 } / label_lit {CVLabelLit $1} / variable { CVVariable $1 }

variable :: Variable
 = identifier { Variable $1 }

atomic_value :: AtomicValue
  = (txt / number)

label_lit :: LabelLit
  = '&' identifier { LabelLit $1 }

identifier :: Text
  = ([_+]+[_+:]* { $1 ++ $2})? [a-zA-Z] [a-zA-Z0-9_:$!?%=-]* { pack ((fromMaybe "" $1) ++ ($2:$3)) } /
  [!@$%^*_=\'`/?×÷≠→←⇒⇐⧺⧻§∘≢∨∪∩□⊃∈+-]+ [:~!@$%^*_=\'`/?×÷≠→←⇒⇐⧺⧻§∘≢∨∪∩□⊃∈+-]* { pack $ $1 ++ $2 } /
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

txt :: AtomicValue
  = sstring / qstring

nl :: String
  = ('\n' '\r' / '\n' / '.') { "" }

sstring :: AtomicValue
  = '#' '[' ([^\]\\] / sstring_escapes)* ']' { TextValue $ pack $1 }

qstring :: AtomicValue
  = ('\"' (
      ('#' '{' code? '}' { maybe (Right $ pack "") (\x -> Left x) $1 }) /
      ((qstring_escapes / [^#\"\\])+ { Right $ pack $1 })
    )* '\"') { InterpolatedTextValue $1 } --"

number :: AtomicValue
  = kdecimal
  / hinteger
  / dinteger

sign :: Maybe Char
  = [-+]?

kdecimal :: AtomicValue
  = sign [0-9]+ '.' [0-9]+ { DecimalValue (sigrat $1 ($2 ++ "." ++ $3)) }

dinteger :: AtomicValue
  = sign [0-9]+ { IntValue (sigdec $1 $2) }

hinteger :: AtomicValue
  = sign '0' 'x' [0-9a-fA-F]+ { IntValue (sighex $1 $2) }
|]

sighex = sigthing hexadecimal
sigdec = sigthing decimal
sigrat = sigthing rational

sigthing :: Num n => Reader n -> Maybe Char -> String -> n
sigthing thing (Just sign) mag = case signed thing (pack (sign : mag)) of Right (number, _) -> number
                                                                          _                 -> 0 
sigthing thing _ mag = case thing (pack mag) of Right (number, _) -> number
                                                _                 -> 0 

