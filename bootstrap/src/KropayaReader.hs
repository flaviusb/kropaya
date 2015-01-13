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

quantifier_block :: QuantifierBlock
  = (existential_block / universal_block / lambda_block)

code :: [Statement] = ((expression { JustExpression $1 }) { [$1] } / nl { [] }) ws* nl? { $1 }

expression :: Expression
 = quantifier_block* (code_type { Left $1 } / code_value { Right $1 }) { Expression $1 $2 }

code_type_b :: CodeType
 = atomic_type { CTAtomicType $1 } / row_type { CTRowType $1 } / product_type { CTProductType $1 }
 / sum_type { CTSumType $1 }
 
code_type :: CodeType
 = lambda_type { CTLambdaType $1 } / code_type_b

lambda_arg :: Expression
 = (code_type_b { Left $1 } / code_value_b { Right $1 }) { Expression [] $1 }

lambda_type :: LambdaType
 = lambda_arg (ws? '→' ws? lambda_arg { $3 })+ { LambdaType ($1:$2) }

lambda_value :: LambdaValue
  = ( ('\\' variable_with_type (ws variable_with_type { $2 })* { $1:$2 })? { fromMaybe [] $1 } ) ws? '⇒' ws? code { LambdaValue $1 $4 }

variable_with_type :: Binding
  = variable ws? ':' ':' ws? expression { Binding $1 $4 }

atomic_type :: AtomicType
 = (('I' 'n' 't' 'e' 'g' 'e' 'r' { IntType }) / ('D' 'e' 'c' 'i' 'm' 'a' 'l' { DecimalType })
 / ('T' 'e' 'x' 't' { TextType }) / ('B' 'i' 'n' 'a' 'r' 'y' { BinaryType })
 / ('S' 'y' 'm' 'b' 'o' 'l' { SymbolType }) / ('B' 'o' 'o' 'l' 'e' 'a' 'n' { BooleanType }))

code_value_b :: CodeValue
 = atomic_value { CVAtomicValue $1 } / label_lit { CVLabelLit $1 } / variable { CVVariable $1 }
 / product_value { CVProductValue $1 } / sum_value { CVSumValue $1 }

code_value :: CodeValue
 = lambda_value { CVLambdaValue $1 } / code_value_b

variable :: Variable
 = identifier { Variable $1 }

atomic_value :: AtomicValue
  = (txt / number)

label_lit :: LabelLit
  = '&' identifier { LabelLit $1 }

label_bit :: LabelBit
 = label_lit { LabelLitBit $1 } / variable { LabelVarBit $1 }

label_section_type :: LabelSectionType
 = label_bit ws? ':' ':' ws? expression { LabelSectionType $1 $4 }

label_section_value :: LabelSectionValue
 = label_bit ws? '⇒' ws? expression { LabelSectionValue $1 $4 }

label_section_type_list :: [LabelSectionType]
 = label_section_type (ws? ',' ws? label_section_type { $3 })* { $1:$2 }

label_section_value_list :: [LabelSectionValue]
 = label_section_value (ws? ',' ws? label_section_value { $3 })* { $1:$2 }

row_type :: RowType
 = '⦇' ws? label_section_type* ws? '⦈' { RowType $2 }

product_type :: ProductType
 = '{' ws? label_section_type_list ws? '}' { ProductType $2 }

sum_type :: SumType
 = '<' ws? label_section_type_list ws? '>' { SumType $2 }

product_value :: ProductValue
 = '{' ws? label_section_value_list ws? '}' { ProductValue $2 }

sum_value :: SumValue
 = '<' ws? label_section_value ws? '>' { SumValue $2 }

identifier :: Text
  = ([_+]+[_+]* { $1 ++ $2})? [a-zA-Z] [a-zA-Z0-9_$!?%=-]* { pack ((fromMaybe "" $1) ++ ($2:$3)) } /
  [!@$%^*_=\'`/?×÷≠←⇐⧺⧻§∘≢∨∪∩□⊃∈$+-]+ [~!@$%^*_=\'`/?×÷≠←⇐⧺⧻§∘≢∨∪∩□⊃∈$+-]* { pack $ $1 ++ $2 } /
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

