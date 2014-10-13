module TypeCheck where

import AST
import Eval (GlobalCtxt, interpOp, interpCmp)

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, put, get, runState)
import Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT)

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Debug.Trace (trace)

type Arity = Int

data Error = DataDeclScopeError NTyCon NDataCon
  | DuplicateDefinition String
  deriving Show

-- elaborate data declaration
elabDataDef :: NTyCon -> DataDefn -> Either String DataDef
elabDataDef tyN@(NTyCon tyName) (DataDefn vars dataAlts) = 
  case [ DataDeclScopeError tyN dN
       | DataAlt dN exprs <- dataAlts, e <- exprs
       , not (S.null (freeVarsQ (Q varSet e))) ] of
    [] -> Right (DataDef tyName vars' dataAlts)
    errs -> Left (show errs)
  where
  vars' = map (\(NTerm n) -> n) vars
  varSet = S.fromList vars'

data DataDef = DataDef String [String] [DataAlt] deriving Show

dataDefCtxt :: DataDef -> Context
dataDefCtxt (DataDef tyName vars dataAlts) = 
  (Context M.empty funcs (M.singleton tyName arity))
  where
  arity = S.size varSet
  varSet = S.fromList vars
  funcs = M.fromList [ (dcon, (DataCon, Q varSet (x, tyExpr)))
    | DataAlt (NDataCon dcon) x <- dataAlts ]
  varsE = map TyVar vars
  tyExpr = TAp (NTyCon tyName) varsE

data Context  = Context
  { terms  :: Map String (Quantified TyExpr)
  , funcs  :: Map String (Inj, Quantified ([TyExpr], TyExpr))
  , tycons :: Map String Arity
  }

instance Show Context where
  show = pprintContext

pprintContext :: Context -> String
pprintContext (Context ts fs tycs) = intercalate "\n" 
  . intercalate ["\n"]
  $ [mp ppTyCons tycs, mp ppFunc fs, mp ppTerm ts]
  where
  mp f = M.elems . M.mapWithKey f
  ppTerm name (Q vars ty) = ppQuantified vars ++ name ++ " : " 
    ++ pprintTyExpr ty
  ppFunc name (inj, Q vars (args, ret)) = ppQuantified vars ++ fType 
    ++ " " ++ name ++ "(" ++ intercalate ", " (map pprintTyExpr args)
    ++ ") : " ++ pprintTyExpr ret
    where
    fType = case inj of 
      { DataCon -> "data constructor"; Func -> "function" }
  ppTyCons name arity = "type constructor " ++ name ++ 
    "(" ++ intercalate ", " (take arity (map (:[]) ['a'..])) ++ ")"
  ppQuantified vars = "" --"For all " ++ concatMap (++ ", ") (S.toList vars)

unionC :: Context -> Context -> Either String Context
unionC (Context a b c) (Context a' b' c') = if M.null iAll
  then Right $ Context (M.union a a') (M.union b b') (M.union c c')
  else Left (show (M.elems iAll))
  where
  ia = M.intersectionWithKey err a a'
  ib = M.intersectionWithKey err b b'
  ic = M.intersectionWithKey err c c'
  iAll = M.unions [ia, ib, ic]
  err key _ _ = DuplicateDefinition key

data Inj = Func | DataCon deriving Show

data Quantified a = Q (Set String) a deriving Show

type TypeEnv = Map NTerm (Quantified TyExpr)

mono :: TyExpr -> Quantified TyExpr
mono = Q S.empty

type Subst = Map String TyExpr

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
g `composeSubst` f = g `M.union` M.map (apply g) f

apply :: Subst -> TyExpr -> TyExpr
apply = M.foldr (.) id . M.mapWithKey subst


strictZipWith :: (a -> b -> c) -> [a] -> [b] -> Either (Int, Int) [c]
strictZipWith f = g (0 :: Int) where
  g _ [] [] = Right []
  g n [] xs@(_ : _) = Left (n, n + length xs)
  g n xs@(_ : _) [] = Left (n + length xs, n)
  g n (x : xs) (y : ys) = fmap (f x y :) (g (n + 1) xs ys)

union' :: TyExpr -> TyExpr -> TypeCheck TyExpr
union' ty1 ty2 = do
  ty1' <- evalTy ty1
  ty2' <- evalTy ty2
  union ty1' ty2'


evalTy :: TyExpr -> TypeCheck TyExpr
evalTy ty = do
  (TIState _ subst) <- lift get
  return (apply subst ty)

union :: TyExpr -> TyExpr -> TypeCheck TyExpr
union (TAp tcon1@(NTyCon n1) e1s) (TAp tcon2@(NTyCon n2) e2s) = if tcon1 == tcon2
  then do
    let Right unionOps = strictZipWith union e1s e2s
    argTys <- sequence unionOps
    return (TAp tcon1 argTys)
  else hoistEither $ Left ("Type constructors " ++ n1 ++ " and "
        ++ n2 ++ " don't match")
union (TyVar u) t = varBind u t
union t (TyVar u) = varBind u t
union IntTy IntTy = return IntTy
union StrTy StrTy = return StrTy
union t1 t2 = hoistEither $ Left ("Can't match type " ++ show t1
  ++ " with " ++ show t2)

varBind :: String -> TyExpr -> TypeCheck TyExpr
varBind u t = case t of
  TyVar u' | u == u' -> return t
  _ -> if S.member u (freeVars t)
    then hoistEither $ Left "Occurs check error"
    else do
      addSubst u t
      return t

addSubst :: String -> TyExpr -> TypeCheck ()
addSubst u t = do
  TIState i subst <- lift get
  lift $ put (TIState i (M.singleton u t `composeSubst` subst))

freeVarsQ :: Quantified TyExpr -> Set String
freeVarsQ (Q vars tyExpr) = freeVars tyExpr S.\\ vars

freeVars :: TyExpr -> Set String
freeVars ty = case ty of
  TyVar a -> S.singleton a
  TAp dcon exprs -> S.unions (map freeVars exprs)
  _ -> S.empty

alphaConvert :: (Set String) -> Quantified TyExpr -> Quantified TyExpr
alphaConvert set1 (Q set2 ty) = 
  (foldr (.) id . map f $ S.toList set2) (Q S.empty ty)
  where
  f name (Q set thisty) = 
    let name' = uniqueName name (S.union set1 set) in
    Q (S.insert name' set) (subst name (TyVar name') thisty)
  
instantiate :: ((TyExpr -> TyExpr) -> (a -> a)) -> 
  Quantified a -> TypeCheck a
instantiate mapf (Q vars expr) = do
  nvars <- mapM (\x -> (,) x <$> newTyVar) (S.toList vars)
  return (mapf (apply (M.fromList nvars)) expr)

instantiateE :: Quantified TyExpr -> TypeCheck TyExpr
instantiateE = instantiate id

instantiateFunc :: Quantified ([TyExpr], TyExpr) -> TypeCheck ([TyExpr], TyExpr)
instantiateFunc = instantiate mapf where
  mapf f (argTys, retTy) = (map f argTys, f retTy)

data TIState = TIState Int Subst deriving Show

newTyVar :: TypeCheck TyExpr
newTyVar = do
  (TIState i s) <- lift get
  lift $ put (TIState (i + 1) s)
  return (TyVar ("a" ++ show i))

type TypeCheck = EitherT String (State TIState)

ti :: Context -> Expr -> TypeCheck TyExpr
ti ctxt e = case e of
  EInt _ -> return IntTy
  EStr _ -> return StrTy
  EVar vname -> case M.lookup vname (terms ctxt) of
    Nothing -> hoistEither $ Left ("Variable " ++ vname ++ " not in scope")
    Just scheme -> do
      ty <- instantiateE scheme
      return ty
  EConstrAp (NDataCon dcon) exprs -> 
    ti ctxt (EAp (NTerm dcon) exprs)
  EAp (NTerm fname) exprs -> case M.lookup fname (funcs ctxt) of
    Nothing -> hoistEither $ Left ("Function " ++ fname ++ " not in scope")
    Just (_, scheme) -> do
      (argTys, retTy) <- instantiateFunc scheme
      argTys2 <- mapM (ti ctxt) exprs
      trace ("argTys: " ++ show argTys ++ "\nargTys2: " ++ show argTys2)
        True `seq` return ()
      case strictZipWith union' argTys argTys2 of
        Left (i, j) -> hoistEither $ Left ("Function " ++ fname ++ " has " ++ show i
          ++ " arguments, but received " ++ show j)
        Right toUnion -> sequence toUnion
      return retTy
  ELet (TypedIdent (NTerm name) _) assn expr -> do
    assnTy <- ti ctxt assn
    ti (ctxt { terms = M.insert name (Q S.empty assnTy) (terms ctxt) }) expr
  ECase expr dataAlts -> do
    ety <- ti ctxt expr
    (inTys, outTys) <- unzip <$> mapM (tiCase ctxt ety) dataAlts
    outTy <- newTyVar
    _ <- foldM union' ety inTys
    resultTy <- foldM union' outTy outTys 
    return resultTy
  EIntBinOp op e1 e2 -> ti ctxt (EAp (NTerm (interpOp op)) [e1, e2])
  EIntBinCmp cmp e1 e2 -> ti ctxt (EAp (NTerm (interpCmp cmp)) [e1, e2])
  ENegate e -> ti ctxt (EAp (NTerm "negate") [e])

tiCase :: Context -> TyExpr -> Production Expr 
       -> TypeCheck (TyExpr, TyExpr)
tiCase ctxt ety 
  (Production (Pattern (NDataCon dcon) argNames) outExpr) = 
    trace ("scrut type:" ++ show ety) True `seq`
    case M.lookup dcon (funcs ctxt) of
      Nothing -> hoistEither $ Left ("Data constructor " ++ dcon ++
        " not in scope")
      Just (Func, _) -> hoistEither $ Left ("Expected data constructor" ++
        " in case alternative but found function " ++ dcon)
      Just (DataCon, scheme) -> do
        (argTys, retTy) <- instantiateFunc scheme
        case strictZipWith zipper argNames argTys of
          Left (i, j) -> hoistEither $ Left ("Data constructor " ++ dcon ++
            " expects " ++ show j ++ " arguments but receives " ++ show i
            ++ " in case alternative")
          Right vs -> do
            outTy <- ti (updateCtxt vs) outExpr
            return (retTy, outTy)
  where
  zipper (NTerm x) y = (x, Q S.empty y)
  updateCtxt :: [(String, Quantified TyExpr)] -> Context
  updateCtxt vs = 
    ctxt { terms = (foldr (.) id (map (uncurry M.insert) vs)) (terms ctxt) }

funcCtxt :: Context -> NTerm -> FuncDefn -> Either String Context
funcCtxt ctxt fname@(NTerm fn) fdef = fmap f (elabFunction ctxt fname fdef)
  where
  f :: Quantified ([TyExpr], TyExpr) -> Context
  f ty = Context M.empty (M.singleton fn (Func, ty)) M.empty

elabFunction :: Context -> NTerm -> FuncDefn -> Either String 
  (Quantified ([TyExpr], TyExpr))
elabFunction ctxt fname fdef = fmap generalize result
  where
  (result, _) = runTypeCheck (tiFunc ctxt fname fdef)

generalize :: ([TyExpr], TyExpr) -> Quantified ([TyExpr], TyExpr)
generalize (argTys, retTy) = Q vars (argTys, retTy) where
  vars = S.unions (map freeVars (retTy : argTys))

tiFunc :: Context -> NTerm -> FuncDefn
  -> TypeCheck ([TyExpr], TyExpr)
tiFunc ctxt (NTerm fname) (FuncDefn args _ expr) = do
  vs <- mapM newArg args 
  expRetTy <- newTyVar
  actRetTy <- ti (updateCtxt vs expRetTy) expr
  retTy <- union' expRetTy actRetTy
  vs' <- mapM evalTy (map snd vs)
  return (vs', retTy)
  where
  newArg (TypedIdent (NTerm x) _) = do
    var <- newTyVar
    return (x, var)
  updateCtxt :: [(String, TyExpr)] -> TyExpr -> Context
  updateCtxt vs retTy = 
    ctxt { terms = addTerms (terms ctxt)
         , funcs = M.insert fname fdef (funcs ctxt) }
    where
    addTerms = foldr (.) id $ map (\(x, y) -> M.insert x (Q S.empty y)) vs
    fdef = (Func, Q S.empty (map snd vs, retTy))
         

  

runTypeCheck :: TypeCheck a -> (Either String a, TIState)
runTypeCheck t = runState (runEitherT t) initTIState
  where 
  initTIState = TIState 0 M.empty

subst :: String -> TyExpr -> (TyExpr -> TyExpr)
subst a e ty = case ty of
  TyVar a' -> if a == a' then e else ty
  TAp tycon exprs -> TAp tycon (map (subst a e) exprs)
  _ -> ty

nextName :: String -> String
nextName = (++ "'")

uniqueName :: String -> Set String -> String
uniqueName s set = if s `S.member` set
  then uniqueName (nextName s) set
  else s

