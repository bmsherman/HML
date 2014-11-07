module Test where

import AST (Decl (FuncDecl))
import Eval (Value, EvalInterp (..), Evaluator (..), interpreter)
import Parse (parseDecls)
import Primitives (preludeContext)
import Typecheck (Context)

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

makeContext :: Evaluator -> [Decl] -> Map String EvalInterp
makeContext (Evaluator primOps compile) = 
  M.union prims  . M.fromList . mapMaybe f
  where
  f (FuncDecl name defn) = Just (name, compile defn)
  f _ = Nothing
  prims = M.mapKeys ('_' :) primOps

