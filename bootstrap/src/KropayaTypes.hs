module KropayaTypes(Atomic(..), Row(..), Label(..), Predicate(..), Lambda(..), Arglist(..), Code(..), R0Type(..)) where

import Numeric
import Data.Text
import Data.Text.Read
import Data.Maybe

data Atomic   = KInt     Integer
              | KDec     Double
              | KSSt     Text
              | KQSt     [Either [Code] Text]
              | KSym     Text     deriving (Show, Eq)

data Row      = Row [Label] [Row] deriving (Show, Eq)
data Label    = Pinned Text R0Type
              | Polymorphic Text R0Type
              | Literal Text R0Type deriving (Show, Eq)

data Predicate = Lacks Label
               | Equals Row

data TRow     = Product Row
              | Sum Row deriving (Show, Eq)

data Lambda   = Lambda Arglist Code deriving (Show, Eq)
data Arglist  = Arglist [Text] deriving (Show, Eq)
data Code     = KR0Type R0Type
              | NL             deriving (Show, Eq)

data R0Type   = KAtomic  Atomic
              | KRow     TRow
              | KLambda  Lambda  deriving (Show, Eq)
