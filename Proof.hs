module Proof(Proof,
             emptyProof,
             alreadyProved) where

import Data.Map as M

import SimplyTypedLambdaCalculus

data Proof = Proof [ProofLine] Context
     deriving (Eq, Ord, Show)

emptyProof :: Proof
emptyProof = Proof [] emptyContext

alreadyProved :: Type -> Proof -> Bool
alreadyProved t pf = False

-- IGNORING LINES FOR NOW
--makeProof d ls c = Proof d [] c

data ProofLine
     = ByVar Declaration
     | ByAppl Declaration
     | ByAbst Declaration
     | IDK Declaration
       deriving (Eq, Ord, Show)

data Context = Context (Map Type TermVariable)
     deriving (Eq, Ord, Show)

emptyContext :: Context
emptyContext = Context M.empty

addVar :: Type -> TermVariable -> Context -> Context
addVar t tv (Context m) = Context (M.insert t tv m)

contains t (Context m) = M.member t m

declaration t (Context m) =
  case v of
    Just var -> show var ++ " : " ++ show t
    Nothing -> error $ show v ++ " does not appear in context " ++ show m
  where
    v = M.lookup t m
