module SimplyTypedLambdaCalculus(Type,
                                 Term,
                                 TermVariable,
                                 Declaration,
                                 var,
                                 app,
                                 abst,
                                 decl) where

data TermVariable = TermVariable String
     deriving (Eq, Ord)

instance Show TermVariable where
  show (TermVariable n) = n

data Term
  = Variable TermVariable
  | Application Term Term
  | Abstraction TermVariable Type Term
    deriving (Eq, Ord)

var = Variable
app = Application
abst = Abstraction

instance Show Term where
  show (Variable v) = show v
  show (Application left right) = "(" ++ show left ++ " " ++ show right ++ ")"
  show (Abstraction v t m) = "(" ++ "\\" ++ show v ++ " : " ++ show t ++ " . " ++ show m ++ ")"

typeVar (Variable t) = t

data Type
  = TypeVariable String
  | Arrow Type Type
    deriving (Eq, Ord)

tv = TypeVariable
ar = Arrow

instance Show Type where
  show (TypeVariable n) = n
  show (Arrow left right) = "(" ++ (show left) ++ " -> " ++ (show right) ++ ")"

data Declaration = Decl Term Type
     deriving (Eq, Ord)

decl tm tp = Decl tm tp
term (Decl tm _) = tm

instance Show Declaration where
  show (Decl tm tp) = show tm ++ " : " ++ show tp

