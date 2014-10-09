module Test where

import Lex (runLex)
import Parse (parse)

import AST
import Eval
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

test :: FilePath -> IO (Either String Value)
test fp = do
  src <- readFile fp
  let decls = parse (runLex src)
  let gctxt = makeContext interpreter decls
  let EvalInterp mainFunc = gctxt M.! (NTerm "main")
  return $ mainFunc gctxt []

makeContext :: Evaluator EvalInterp Value -> [Decl] -> GlobalCtxt
makeContext (Evaluator primOps compile) = 
  M.union prims  . M.fromList . mapMaybe f
  where
  --f :: Decl -> Maybe (NTerm, FuncDefn)
  f (FuncDecl name defn) = Just (name, compile defn)
  f _ = Nothing
  prims = M.mapKeys (\(NTerm x) -> NTerm ('_' : x)) primOps
