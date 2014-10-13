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

import Debug.Trace (traceShow)

test :: FilePath -> IO (Either String [Decl])
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
  

tyCheckTest :: Expr -> (Either String TyExpr, TIState)
tyCheckTest e = runTypeCheck (evalTy =<< ti primContext e)

--tyCheckMain :: [Decl] -> (Either String TyExpr, TIState)
tyCheckMain decls = traceShow (mapMaybe g decls) True `seq`
  runTypeCheck (tiFunc ctxt mainN mainF)
  where
  ((mainN, mainF):_) = mapMaybe getMain decls
  Right ctxt = foldM unionC primContext (mapMaybe f decls)
  g (DataDecl tycon datadef) =
    Just (elabDataDef tycon datadef)
  g _ = Nothing
  f (DataDecl tycon datadef) =
    fmap dataDefCtxt (eToM (elabDataDef tycon datadef))
  f _ = Nothing
  getMain (FuncDecl n@(NTerm "main") funcDef) = Just (n, funcDef)
  getMain _ = Nothing
  eToM (Right x) = Just x
  eToM (Left _) = Nothing

myExpr :: Expr
myExpr = EAp (NTerm "_+") [EInt 10, EInt 20]
