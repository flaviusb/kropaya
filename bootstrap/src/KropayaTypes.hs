module KropayaTypes(Atomic(..), Row(..), Label(..), Predicate(..), RowOp(..), TRow(..), Lambda(..), Arglist(..), Code(..), R0Type(..), Variable(..), QuantifierBlock(..), LabelLit(..), LabelBit(..), LabelSectionType(..), LabelSectionValue(..), RowType(..), ProductType(..), ProductValue(..), SumType(..), SumValue(..), LambdaType(..), LambdaValue(..), AtomicType(..), AtomicValue(..), CodeType(..), CodeValue(..), Expression(..), Statement(..), Program(..)) where

import Numeric
import Data.Text
import Data.Text.Read
import Data.Maybe
import qualified Data.ByteString as BS

-- 'Untyped' first AST

data Variable  = Variable Text deriving (Show, Eq)
data QuantifierBlock = UniversalBlock [Variable]
                     | ExistentialBlock [Variable]
                     | LambdaBlock [Variable] deriving (Eq, Show)

data LabelLit = LabelLit Text deriving (Show, Eq)
data LabelBit = LabelLitBit LabelLit
              | LabelVarBit Variable deriving (Show, Eq)
data LabelSectionType  = LabelSectionType LabelBit Expression deriving (Show, Eq)
data LabelSectionValue = LabelSectionValue LabelBit Expression deriving (Show, Eq)
data RowType = RowType [LabelSectionType] deriving (Show, Eq)
data ProductType = ProductType [LabelSectionType] deriving (Show, Eq)
data SumType = SumType [LabelSectionType] deriving (Show, Eq)
data ProductValue = ProductValue [LabelSectionValue] deriving (Show, Eq)
data SumValue = SumValue [LabelSectionValue] deriving (Show, Eq)
data LambdaType = LambdaType [Expression] deriving (Show, Eq)
data LambdaValue = LambdaValue [Statement] deriving (Show, Eq)
data AtomicType = IntType | DecimalType | TextType | BinaryType | SymbolType deriving (Show, Eq)
data AtomicValue = IntValue Integer
                 | DecimalValue Double
                 | TextValue Text
                 | InterpolatedTextValue [Either [Statement] Text]
                 | BinaryValue BS.ByteString
                 | SymbolValue Text deriving (Show, Eq)

data CodeType = CTLabelLit    LabelLit
              | CTVariable    Variable
              | CTRowType     RowType
              | CTProductType ProductType
              | CTSumType     SumType
              | CTLambdaType  LambdaType
              | CTAtomicType  AtomicType deriving (Show, Eq)

data CodeValue = CVLabelLit     LabelLit
               | CVVariable     Variable
               | CVProductValue ProductValue
               | CVSumValue     SumValue
               | CVLambdaValue  LambdaValue
               | CVAtomicValue  AtomicValue deriving (Show, Eq)


data Expression = Expression [QuantifierBlock] (Either CodeType CodeValue) deriving (Show, Eq)
data Statement = JustExpression Expression
               | Binding Variable Expression
               | Typing  Variable Expression deriving (Show, Eq)

data Program = Program [Statement] deriving (Show, Eq)

--- 'Typed' Second AST

data Atomic    = KInt     Integer
               | KDec     Double
               | KSSt     Text
               | KQSt     [Either [Code] Text] -- This is an interpolated string, made of Text and Code segments
               | KSym     Text     deriving (Show, Eq)

data Row       = Row [Label] deriving (Show, Eq)
data Label     = Pinned Text R0Type
               | Polymorphic Text R0Type
               | Literal Text R0Type deriving (Show, Eq)

data Predicate = Lacks Row Label
               | Equals Row Row deriving (Show, Eq)

data RowOp     = Extend   Row Label
               | Restrict Row Label deriving (Show, Eq)

data TRow      = Product Row
               | Sum Row deriving (Show, Eq)

data Lambda    = Lambda Arglist Code deriving (Show, Eq)
data Arglist   = Arglist [Text] deriving (Show, Eq)
data Code      = KR0Type R0Type
               | NL             deriving (Show, Eq)

-- R0Types are non-dependent principle types; dependent principle types will be indexed to avoid girards paradox a la ramified types
-- This is a giant TODO, really :-(
data R0Type   = KAtomic    Atomic
              | KRow       TRow
              | KLabel     Label
              | KPredicate Predicate
              | KRowOp     RowOp
              | KLambda    Lambda  deriving (Show, Eq)
