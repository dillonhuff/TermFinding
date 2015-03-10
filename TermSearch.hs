module TermSearch() where

import Proof
import SimplyTypedLambdaCalculus

saturate :: [Declaration] -> [Declaration] -> [Declaration]
--saturate _ _ = error "NNOOOOOO"
saturate old [] = []
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


data VarSource = VarSource Int

newSource = VarSource 0

freshVar :: VarSource -> (VarSource, Term)
freshVar (VarSource ind) = (VarSource (ind+1), Variable$ TermVariable $ "x" ++ show ind)

findTerm :: Type -> Maybe Proof
findTerm t = findProof newSource t emptyContext emptyProof

findProof :: VarSource -> Type -> Context -> Proof -> Maybe Proof
findProof vs v@(TypeVariable x) c pf = case alreadyProved v pf of
  True -> Just pf
  False -> Nothing
findProof vs (Arrow l r) c ds = --error "FIND ARROW PROOF"
  let (newVS, fr) = freshVar vs
      newRes = saturate ds [decl fr l]
      arrowPf = findProof newVS r (addVar l (typeVar fr) c) (ds ++ newRes)
      in
  case arrowPf of
    Just pf -> Just $ Proof (decl (Abstraction (typeVar fr) l (term $ result pf)) (Arrow l r)) [] c
    _ -> Nothing
