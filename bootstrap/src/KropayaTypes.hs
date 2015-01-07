module KropayaTypes(Atomic(..), Row(..), Label(..), Predicate(..), RowOp(..), TRow(..), Lambda(..), Arglist(..), Code(..), R0Type(..), Variable(..), QuantifierBlock(..)) where

import Numeric
import Data.Text
import Data.Text.Read
import Data.Maybe

-- 'Untyped' first AST

data Variable  = Variable Text deriving (Show, Eq)
data QuantifierBlock = UniversalBlock [Variable]
                     | ExistentialBlock [Variable]
                     | LambdaBlock [Variable] deriving (Eq, Show)

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
