module Primitives where

import AST
import Paths_HW3 (getDataFileName)
import Parse (parseDecls)
import Typecheck

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import System.IO.Unsafe (unsafePerformIO)

type FuncTy = Quantified ([TyExpr], TyExpr)

intBinOps :: Map String (Inj, FuncTy)
intBinOps = M.fromList [ (n,  (Func, ty)) | n <- names ] where
  ty = Q S.empty ([IntTy, IntTy], IntTy)
  names = ["_+", "_*", "_-"]

intCmpOps :: Map String (Inj, FuncTy)
intCmpOps = M.fromList [ (n,  (Func, ty)) | n <- names ] where
  ty = Q S.empty ([IntTy, IntTy], boolTy)
  names = ["_<=", "_<", "_==", "_>", "_>="]

boolTy :: TyExpr
boolTy = TAp (NTyCon "Bool") []

primOps :: Map String (Inj, FuncTy)
primOps = M.unions [intBinOps ,neg, intCmpOps ]
  where
  neg = M.fromList [("_negate", (Func, Q S.empty ([IntTy], IntTy)))]

-- | A typing context with only primitive operations included
primContext :: Context
primContext = Context M.empty primOps M.empty

-- | A typing context that includes built-in functions from a Prelude
-- code file
preludeContext :: Context
preludeContext = ctxt
  where
  preludeSrc = unsafePerformIO $ 
    readFile =<< getDataFileName "Prelude.hm"
  Right (_, (ctxt, _)) = parseDecls primContext preludeSrc
