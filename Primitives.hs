module Primitives where

import AST
import TypeCheck

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

type FuncTy = Quantified ([TyExpr], TyExpr)

intBinOps :: Map String (Inj, FuncTy)
intBinOps = M.fromList [ (n,  (Func, ty)) | n <- names ] where
  ty = Q S.empty ([IntTy, IntTy], IntTy)
  names = ["_+", "_*", "_-"]

intCmpOps :: Map String (Inj, FuncTy)
intCmpOps = M.fromList [ (n,  (Func, ty)) | n <- names ] where
  ty = Q S.empty ([IntTy, IntTy], boolTy)
  names = ["_<=", "_<", "_=="]

boolTy :: TyExpr
boolTy = TAp (NTyCon "Bool") []

primOps :: Map String (Inj, FuncTy)
primOps = M.unions [intBinOps ,neg, intCmpOps ]
  where
  neg = M.fromList [("_negate", (Func, Q S.empty ([IntTy], IntTy)))]
  ident = M.fromList
    [ ("_id", (Func, Q (S.singleton "a") ([tyVar "a"], tyVar "a")))
    , ("_id2", (Func, Q (S.fromList ["a", "b"]) 
         ([tyVar "a", tyVar "b"], tyVar "b")))
    ]
  tyVar = TyVar . TV Flex

primContext :: Context
primContext = Context M.empty primOps M.empty
