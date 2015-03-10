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

typeVar (Variable t) = t

data Type
  = TypeVariable String
  | Arrow Type Type
    deriving (Eq, Ord)

instance Show Type where
  show (TypeVariable n) = n
  show (Arrow left right) = "(" ++ (show left) ++ " -> " ++ (show right) ++ ")"

data Declaration = Decl Term Type
     deriving (Eq, Ord)

decl tm tp = Decl tm tp
term (Decl tm _) = tm

instance Show Declaration where
  show (Decl tm tp) = show tm ++ " : " ++ show tp

data Context = Context (Map Type TermVariable)
     deriving (Eq, Ord, Show)

emptyContext :: Context
emptyContext = Context M.empty

addVar :: Type -> TermVariable -> Context -> Context
addVar t tv (Context m) = Context (M.insert t tv m)

contains t (Context m) = M.member t m

var t (Context m) = case M.lookup t m of
  Just tv -> Variable tv
  Nothing -> error $ show t ++ " does not appear in context " ++ show m

declaration t (Context m) =
  case v of
    Just var -> show var ++ " : " ++ show t
    Nothing -> error $ show v ++ " does not appear in context " ++ show m
  where
    v = M.lookup t m

data Proof = Proof Declaration [ProofLine] Context
     deriving (Eq, Ord, Show)

-- IGNORING LINES FOR NOW
makeProof d ls c = Proof d [] c

result (Proof ds _ _) = ds

saturate :: [Declaration] -> [Declaration] -> [Declaration]
saturate old new = let newResults = findAppls old new in
  newResults ++ (saturate (old ++ new) newResults)

findAppls :: [Declaration] -> [Declaration] -> [Declaration]
findAppls old new = appliedRes
  where
    oldNewPairs = [(x, y) | x <- old, y <- new]
    appliedRes = concatMap tryApply oldNewPairs

tryApply :: (Declaration, Declaration) -> [Declaration]
tryApply (Decl t1 (Arrow l r), Decl t2 tp) = case tp == l of
  True -> [Decl (Application t1 t2) r]
  False -> []
tryApply (Decl t1 tp, Decl t2 (Arrow l r)) = case tp == l of
  True -> [Decl (Application t1 t2) r]
  False -> []
tryApply _ = []

data ProofLine
     = ByVar Declaration
     | ByAppl Declaration Declaration Declaration
     | ByAbst Declaration TermVariable Type Proof
       deriving (Eq, Ord, Show)

data VarSource = VarSource Int

newSource = VarSource 0

freshVar :: VarSource -> (VarSource, Term)
freshVar (VarSource ind) = (VarSource (ind+1), Variable$ TermVariable $ "x" ++ show ind)

findProof :: VarSource -> Type -> Context -> [Declaration] -> Maybe Proof
findProof vs v@(TypeVariable x) c ds = case contains v c of
  True -> Just $ makeProof (decl (var v c) v) ds c
  False -> Nothing
findProof vs (Arrow l r) c ds =
  let (newVS, fr) = freshVar vs
      newRes = saturate ds [decl fr l]
      arrowPf = findProof newVS r (addVar l (typeVar fr) c) (ds ++ newRes) in
  case arrowPf of
    Just pf -> Just $ Proof (decl (Abstraction (typeVar fr) l (term $ result pf)) (Arrow l r)) [] c
    _ -> Nothing
