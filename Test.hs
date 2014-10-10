module Test where

import Parse (parseDecls)

import AST
import Eval
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

test :: FilePath -> IO (Either String Value)
test fp = do
  src <- readFile fp
  return $ case parseDecls src of
    Left e -> Left e
    Right decls -> evalMain decls

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
