{-| An interpreter for the language. Not commented since this isn't
necessary for this assignment. -}
module Eval where

import AST

import Control.Applicative ((<$>))

import Data.Int (Int32)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M

data Value = VInt Int32
  | VStr String
  | VConstrAp String [Value]
  deriving Show

pprintV :: Value -> String
pprintV x = case x of
  VInt i -> show i
  VStr s -> show s
  VConstrAp dcon vals -> 
    dcon ++ "(" ++ intercalate ", " (map pprintV vals) ++ ")"


data Evaluator = Evaluator 
  { primOps :: Map NTerm EvalInterp
  , compile :: FuncDefn -> EvalInterp
  }

newtype EvalInterp = EvalInterp 
  (Map String EvalInterp -> [Value] -> Either String Value)

interpreter :: Evaluator
interpreter = Evaluator 
  (M.fromList ops)
  (EvalInterp . funEval)
  where
  binOp :: (Int32 -> Int32 -> Int32) -> EvalInterp
  binOp f = EvalInterp $ \_ xs -> case xs of 
      [VInt i, VInt j] -> Right (VInt (f i j))
      _ -> Left "binary operation error"
  binCmp :: (Int32 -> Int32 -> Bool) -> EvalInterp
  binCmp f = EvalInterp $ \_ xs -> case xs of 
      [VInt i, VInt j] -> Right (bool (f i j))
      _ -> Left "binary comparison error"
  neg :: EvalInterp
  neg = EvalInterp $ \_ x -> case x of
    [VInt i] -> Right (VInt (- i))
    _ -> Left "negation error"
  ops =
    [ ("plus", binOp (+))
    , ("times", binOp (*))
    , ("div", binOp div)
    , ("minus", binOp (-))
    , ("ltInt", binCmp (<))
    , ("lteInt", binCmp (<=))
    , ("eqInt", binCmp (==))
    , ("gteInt", binCmp (>=))
    , ("gtInt", binCmp (>))
    , ("negate", neg)
    ]

data EvalContext = Ctxt
  { vars  :: Map NTerm Value
  , funcs :: Map String EvalInterp
  }

insertVar :: NTerm -> Value -> EvalContext -> EvalContext 
insertVar name val (Ctxt vars funcs) = Ctxt (M.insert name val vars) funcs

eval :: EvalContext -> Expr -> Either String Value
eval ctxt e = case e of
  EInt i -> return (VInt i)
  EStr s -> return (VStr s)
  EVar vname -> case M.lookup vname (vars ctxt) of
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

funEval :: FuncDefn -> Map String EvalInterp
  -> [Value] -> Either String Value
funEval f@(FuncDefn args _ body) gctxt vals = eval ctxt body
  where
  lctxt = M.fromList (zipWith f args vals)
  f (TypedIdent term _) v = (term, v)
  ctxt = Ctxt lctxt gctxt

caseEval :: EvalContext -> Value -> [Production Expr] 
  -> Either String Value
caseEval ctxt (VConstrAp _ _) [] = Left "No cases match"
caseEval ctxt@(Ctxt lctxt gctxt) val@(VConstrAp dcon vals) 
  (Production (Pattern pdcon binders) body : prods) = 
    if dcon == pdcon
      then let newvars = M.fromList (zip binders vals) in
	let lctxt' = M.union newvars lctxt in
	eval (Ctxt lctxt' gctxt) body
      else caseEval ctxt val prods
