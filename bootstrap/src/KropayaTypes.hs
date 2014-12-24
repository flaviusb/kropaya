module KropayaTypes(Atomic(..), Row(..), Label(..), Product(..), Sum(..), TRow(..), Lambda(..), Arglist(..), Code(..), R0Type(..)) where

import Numeric
import Data.Text
import Data.Text.Read
import Data.Maybe

data Atomic   = KInt     Integer
              | KDec     Double
              | KStr     Text
              | KSym     Text     deriving (Show, Eq)

data Row      = Row [Label] deriving (Show, Eq)
data Label    = Label Text R0Type deriving (Show, Eq)

data Product  = Product Row deriving (Show, Eq)
data Sum      = Sum Row deriving (Show, Eq)

data TRow     = KProduct Product
              | KSum Sum deriving (Show, Eq)

data Lambda   = Lambda Arglist Code deriving (Show, Eq)
data Arglist  = Arglist [Text] deriving (Show, Eq)
data Code     = R0Type deriving (Show, Eq)

data R0Type   = KAtomic  Atomic
              | KRow     TRow
              | KLambda  Lambda  deriving (Show, Eq)
