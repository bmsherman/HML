module Test where

import Parse (parseDecls)

import AST
import Eval hiding (Context)

import TypeCheck
import Primitives

import Control.Monad (foldM)

import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

test :: FilePath -> IO (Either String ([Decl], Context))
test fp = do
  src <- readFile fp
  return (parseDecls src)

evalMain :: [Decl] -> Either String Value
evalMain decls = mainFunc gctxt [] where
  gctxt = makeContext interpreter decls
  EvalInterp mainFunc = gctxt M.! (NTerm "main")

makeContext :: Evaluator EvalInterp Value -> [Decl] -> GlobalCtxt
makeContext (Evaluator primOps compile) = 
  M.union prims  . M.fromList . mapMaybe f
  where
  --f :: Decl -> Maybe (NTerm, FuncDefn)
  f (FuncDecl name defn) = Just (name, compile defn)
  f _ = Nothing
  prims = M.mapKeys (\(NTerm x) -> NTerm ('_' : x)) primOps


{-
makeTyCtxt :: [Decl] -> Either String Context
makeTyCtxt decls = do
  ctxt <- foldM unionC primContext (mapMaybe getData decls)
  go ctxt (mapMaybe getFunc decls)
  --runTypeCheck (tiFunc ctxt mainN mainF)
  where
  Right ctxt = foldM unionC primContext (mapMaybe getData decls)
  getData (DataDecl tycon datadef) =
    fmap dataDefCtxt (eToM (elabDataDef tycon datadef))
  getData _ = Nothing
  getFunc (FuncDecl n funcDef) = Just (n, funcDef)
  getFunc _ = Nothing
  go ctxt [] = return ctxt
  go ctxt ((n, funcDef) : defs) = do
    addFuncCtxt <- funcCtxt ctxt n funcDef
    ctxt' <- ctxt `unionC` addFuncCtxt
    go ctxt' defs
  eToM (Right x) = Just x
  eToM (Left _) = Nothing
-}
  

tyCheckTest :: Expr -> (Either String TyExpr, TIState)
tyCheckTest e = runTypeCheck (evalTy =<< ti primContext e)

myExpr :: Expr
myExpr = EAp (NTerm "_+") [EInt 10, EInt 20]
