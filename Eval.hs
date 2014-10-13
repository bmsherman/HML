module Eval where

import AST

import Control.Applicative ((<$>))

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M

data Value = VInt Int
  | VStr String
  | VConstrAp NDataCon [Value]
  deriving Show

pprintV :: Value -> String
pprintV x = case x of
  VInt i -> show i
  VStr s -> show s
  VConstrAp (NDataCon dcon) vals -> dcon ++ "(" ++ intercalate ", " (map pprintV vals) ++ ")"


data Evaluator eval value = Evaluator 
  { primOps :: Map NTerm (eval [value] value)
  , compile :: FuncDefn -> (eval [value] value)
  }

newtype EvalInterp a b = EvalInterp (GlobalCtxt -> a -> Either String b)

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

data Context = Ctxt
  { vars  :: Map NTerm Value
  , funcs :: GlobalCtxt
  }

type GlobalCtxt = Map NTerm (EvalInterp [Value] Value)

insertVar :: NTerm -> Value -> Context -> Context
insertVar name val (Ctxt vars funcs) = Ctxt (M.insert name val vars) funcs

eval :: Context -> Expr -> Either String Value
eval ctxt e = case e of
  EInt i -> return (VInt i)
  EStr s -> return (VStr s)
  EVar vname -> case M.lookup (NTerm vname) (vars ctxt) of
    Nothing -> Left ("Variable " ++ show vname ++ "not in scope")
    Just v -> return v
  EConstrAp dcon exprs -> 
    VConstrAp dcon <$> mapM (eval ctxt) exprs
  EAp fname exprs -> do
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
  EIntBinOp op e1 e2 -> eval ctxt (EAp (NTerm (interpOp op)) [e1, e2])
  EIntBinCmp cmp e1 e2 -> eval ctxt (EAp (NTerm (interpCmp cmp)) [e1, e2])
  ENegate e -> eval ctxt (EAp (NTerm "negate") [e])
  ESeq e1 e2 -> eval ctxt e1 >> eval ctxt e2
  Typed e _ -> eval ctxt e
  where
  intError = Left ("Applying integer operation to non-integer")

interpOp :: IntBinOp -> String
interpOp op = case op of
  Plus ->  "_+"
  Minus -> "_-"
  Times -> "_*"
  Div   -> "_/"

interpCmp :: IntBinCmp -> String
interpCmp cmp = case cmp of
  CmpLT -> "_<"
  CmpLEQ -> "_<="
  CmpEQ -> "_=="

bool :: Bool -> Value
bool b = VConstrAp (NDataCon (show b)) []

funEval :: FuncDefn -> GlobalCtxt -> [Value] -> Either String Value
funEval f@(FuncDefn args _ body) gctxt vals = eval ctxt body
  where
  lctxt = M.fromList (zipWith f args vals)
  f (TypedIdent term _) v = (term, v)
  ctxt = Ctxt lctxt gctxt

caseEval :: Context -> Value -> [Production Expr] -> Either String Value
caseEval ctxt (VConstrAp _ _) [] = Left "No cases match"
caseEval ctxt@(Ctxt lctxt gctxt) val@(VConstrAp dcon vals) 
  (Production (Pattern pdcon binders) body : prods) = if dcon == pdcon
    then let newvars = M.fromList (zip binders vals) in
      let lctxt' = M.union newvars lctxt in
      eval (Ctxt lctxt' gctxt) body
    else caseEval ctxt val prods

{-
kNormalize :: Expr -> CExpr
kNormalize e = case e of
  EInt i -> CLit (CInt i)
  EStr s -> CLit (CStr s)
  EVar s -> CVar (NTerm s)
  EConstrAp dcon exprs -> CConstrAp dcon
-}
