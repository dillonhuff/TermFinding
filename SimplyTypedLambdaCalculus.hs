module SimplyTypedLambdaCalculus() where

import Data.Map as M

data TermVariable = TermVariable String
     deriving (Eq, Ord)

instance Show TermVariable where
  show (TermVariable n) = n

data Term
  = Variable TermVariable
  | Application Term Term
  | Abstraction TermVariable Type Term
    deriving (Eq, Ord)

instance Show Term where
  show (Variable v) = show v
  show (Application left right) = "(" ++ show left ++ " " ++ show right ++ ")"
  show (Abstraction v t m) = "(" ++ "\\" ++ show v ++ " : " ++ show t ++ " . " ++ show m ++ ")"

data Type
  = TypeVariable String
  | Arrow Type Type
    deriving (Eq, Ord)

instance Show Type where
  show (TypeVariable n) = n
  show (Arrow left right) = "(" ++ (show left) ++ " -> " ++ (show right) ++ ")"

data Context = Context (Map Type TermVariable)
     deriving (Eq, Ord, Show)

contains t (Context m) = M.member t m

declaration t (Context m) =
  case v of
    Just var -> show var ++ " : " ++ show t
    Nothing -> error $ show v ++ " does not appear in context " ++ show m
  where
    v = M.lookup t m

data Proof
     = ByAssumption Type Context
     | ByAbstraction Type Proof Proof Context
       deriving (Eq, Ord, Show)

prettyProof :: Int -> Proof -> String
prettyProof indentLevel (ByAssumption t c) =
  (replicate indentLevel '\t') ++ "| " ++ (declaration t c)
prettyProof indentLevel (ByAbstraction t l r c) =
  (replicate indentLevel '\t') ++ "| " ++ show t ++ "\n" ++ (prettyProof (indentLevel + 1) l) ++ "\n" ++ (prettyProof (indentLevel + 1) r)
       
emptyContext :: Context
emptyContext = Context M.empty

findProof :: Type -> Maybe Proof
findProof t = recFindProof t emptyContext

recFindProof :: Type -> Context -> Maybe Proof
recFindProof t c = case contains t c of
  True -> Just $ ByAssumption t c
  False -> case t of
    (TypeVariable _) -> Nothing
    (Arrow left right) -> case contains left c of
      True -> error $ "NOOO"
