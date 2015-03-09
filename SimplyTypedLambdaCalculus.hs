module SimplyTypedLambdaCalculus() where

data TermVariable = TermVariable String
     deriving (Eq, Ord, Show)

data Term
  = Variable TermVariable
  | Application Term Term
  | Abstraction TermVariable Type Term
    deriving (Eq, Ord, Show)

data Type
  = TypeVariable String
  | Arrow Type Type
    deriving (Eq, Ord, Show)

