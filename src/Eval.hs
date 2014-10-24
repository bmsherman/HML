{-| An interpreter for the language. Not commented since this isn't
necessary for this assignment. -}
module Eval where

import AST

import Control.Applicative ((<$>))

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M

data Value = VInt Int
  | VStr String
  | VConstrAp String [Value]
  deriving Show

pprintV :: Value -> String
pprintV x = case x of
  VInt i -> show i
  VStr s -> show s
  VConstrAp dcon vals -> dcon ++ "(" ++ intercalate ", " (map pprintV vals) ++ ")"


data Evaluator eval value = Evaluator 
  { primOps :: Map NTerm (eval [value] value)
  , compile :: FuncDefn -> (eval [value] value)
  }

newtype EvalInterp a b = EvalInterp 
  (Map String (EvalInterp [Value] Value) -> a -> Either String b)

interpreter :: Evaluator EvalInterp Value
interpreter = Evaluator 
  (M.fromList ops)
  (EvalInterp . funEval)
  where
  binOp :: (Int -> Int -> Int) -> EvalInterp [Value] Value
  binOp f = EvalInterp $ \_ xs -> case xs of 
      [VInt i, VInt j] -> Right (VInt (f i j))
      _ -> Left "binary operation error"
  binCmp :: (Int -> Int -> Bool) -> EvalInterp [Value] Value
  binCmp f = EvalInterp $ \_ xs -> case xs of 
      [VInt i, VInt j] -> Right (bool (f i j))
      _ -> Left "binary comparison error"
  neg :: EvalInterp [Value] Value
  neg = EvalInterp $ \_ x -> case x of
    [VInt i] -> Right (VInt (- i))
    _ -> Left "negation error"
  ops =
    [ (NTerm "+", binOp (+))
    , (NTerm "*", binOp (*))
    , (NTerm "/", binOp div)
    , (NTerm "-", binOp (-))
    , (NTerm "<", binCmp (<))
    , (NTerm "<=", binCmp (<=))
    , (NTerm "==", binCmp (==))
    , (NTerm "negate", neg)
    ]

data EvalContext to value = Ctxt
  { vars  :: Map NTerm value
  , funcs :: Map String (to [value] value)
  }

type InterpContext = EvalContext EvalInterp Value

insertVar :: NTerm -> value -> EvalContext to value -> EvalContext to value
insertVar name val (Ctxt vars funcs) = Ctxt (M.insert name val vars) funcs

eval :: InterpContext -> Expr -> Either String Value
eval ctxt e = case e of
  EInt i -> return (VInt i)
  EStr s -> return (VStr s)
  EVar vname -> case M.lookup (NTerm vname) (vars ctxt) of
    Nothing -> Left ("Variable " ++ show vname ++ "not in scope")
    Just v -> return v
  EAp DataCon dcon exprs -> 
    VConstrAp dcon <$> mapM (eval ctxt) exprs
  EAp Func fname exprs -> do
    EvalInterp f <- case M.lookup fname (funcs ctxt) of
      Nothing -> Left ("Function " ++ show fname ++ "not in scope")
      Just f' -> return f'
    vals <- mapM (eval ctxt) exprs
    f (funcs ctxt) vals
  ECase expr prods -> do 
    v <- eval ctxt expr
    caseEval ctxt v prods
  ELet (TypedIdent name _) letexpr fullexpr -> do
    v <- eval ctxt letexpr
    eval (insertVar name v ctxt) fullexpr
  Typed e _ -> eval ctxt e
  where
  intError = Left ("Applying integer operation to non-integer")

bool :: Bool -> Value
bool b = VConstrAp (show b) []

funEval :: FuncDefn -> Map String (EvalInterp [Value] Value)
  -> [Value] -> Either String Value
funEval f@(FuncDefn args _ body) gctxt vals = eval ctxt body
  where
  lctxt = M.fromList (zipWith f args vals)
  f (TypedIdent term _) v = (term, v)
  ctxt = Ctxt lctxt gctxt

caseEval :: InterpContext -> Value -> [Production Expr] 
  -> Either String Value
caseEval ctxt (VConstrAp _ _) [] = Left "No cases match"
caseEval ctxt@(Ctxt lctxt gctxt) val@(VConstrAp dcon vals) 
  (Production (Pattern (NDataCon pdcon) binders) body : prods) = 
    if dcon == pdcon
      then let newvars = M.fromList (zip binders vals) in
	let lctxt' = M.union newvars lctxt in
	eval (Ctxt lctxt' gctxt) body
      else caseEval ctxt val prods
