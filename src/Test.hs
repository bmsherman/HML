module Test where

import Parse (parseDecls)

import AST
import Eval

import Typecheck
import Primitives

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

test :: FilePath -> IO (Either String ([Decl], Context))
test fp = do
  src <- readFile fp
  case parseDecls preludeContext src of
    Left e -> return (Left e)
    Right (decls, (ctxt, errors)) -> do
      mapM_ putStrLn errors
      return $ Right (decls, ctxt)

evalMain :: [Decl] -> Either String Value
evalMain decls = mainFunc gctxt [] where
  gctxt = makeContext interpreter decls
  EvalInterp mainFunc = gctxt M.! "main"

makeContext :: Evaluator EvalInterp Value -> [Decl]
  -> Map String (EvalInterp [Value] Value)
makeContext (Evaluator primOps compile) = 
  M.union prims  . M.fromList . mapMaybe f
  where
  --f :: Decl -> Maybe (NTerm, FuncDefn)
  f (FuncDecl (NTerm name) defn) = Just (name, compile defn)
  f _ = Nothing
  prims = M.mapKeys (\(NTerm x) -> ('_' : x)) primOps

