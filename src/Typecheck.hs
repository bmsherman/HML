{-# LANGUAGE Rank2Types #-}
module Typecheck where

import AST

import Control.Applicative ((<$>), (<|>))
import Control.Arrow (second, left)
import Control.Monad (foldM, zipWithM, guard, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, put, get, runState)
import Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT)

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

-- elaborate data declaration
elabDataDef :: NTyCon -> DataDefn -> Either [String] DataDef
elabDataDef (NTyCon tyName) (DataDefn vars dataAlts) = 
  case [ "in data constructor "
         ++ dN ++ ", type variable '" ++ tyVar ++ "' not in scope."
       | DataAlt (NDataCon dN) exprs <- dataAlts, e <- exprs
       , tyVar <- S.toList (freeVarsQ (Q varSet e)) ] of
    [] -> Right (DataDef tyName vars' dataAlts)
    errs -> Left errs
  where
  vars' = map (\(NTerm n) -> n) vars
  varSet = S.fromList vars'

data DataDef = DataDef String [String] [DataAlt] deriving Show

dataDefCtxt :: Map String Arity -> DataDef 
  -> Either [String] Context
dataDefCtxt tycs (DataDef tyName vars dataAlts) = do
  mLefts  [ left (("in data constructor '" ++ dcon ++ "', ") ++) 
             $ kindCheck tycs' e
             | DataAlt (NDataCon dcon) es <- dataAlts, e <- es ]
  return (Context M.empty funcs (M.singleton tyName arity))
  where
  mLefts = go [] where
    go acc [] = if null acc then Right () else Left (reverse acc)
    go acc (x : xs) = case x of
      Left l -> go (l : acc) xs
      Right _ -> go acc xs
  tycs' = M.insert tyName arity tycs
  arity = S.size varSet
  varSet = S.fromList vars
  funcs = M.fromList [ (dcon, (DataCon, Q varSet (x, tyExpr)))
    | DataAlt (NDataCon dcon) x <- dataAlts ]
  varsE = map (TyVar . TV Flex) vars
  tyExpr = TAp (NTyCon tyName) varsE

type Arity = Int
data Context  = Context
  { terms  :: Map String (Quantified TyExpr)
  , funcs  :: Map String (Inj, Quantified ([TyExpr], TyExpr))
  , tycons :: Map String Arity
  }

emptyContext :: Context
emptyContext = Context M.empty M.empty M.empty

instance Show Context where
  show = pprintContext

kindCheck :: Map String Arity -> TyExpr -> Either String ()
kindCheck tycs ty = case ty of
  TAp (NTyCon tcon) es -> case M.lookup tcon tycs of
    Nothing -> Left $ "type constructor '" ++ tcon ++ "' not in scope"
    Just arity -> let l = length es in if l == arity
      then zipWithM_ (\i -> left (("in argument " ++ show i ++ 
           " of type constructor '" ++ tcon ++ "', ") ++) . kindCheck tycs) 
           [1..] es
      else Left $ "type constructor '" ++ tcon ++ "' expects " ++ show arity
        ++ plural arity " argument" ++ " but received " ++ show l
  _ -> Right ()

pprintContext :: Context -> String
pprintContext (Context ts fs tycs) = intercalate "\n" 
  . intercalate [""]
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

unionC :: Context -> Context -> Either [String] Context
unionC (Context a b c) (Context a' b' c') = if M.null iAll
  then Right $ Context (M.union a a') (M.union b b') (M.union c c')
  else Left (M.elems iAll)
  where
  ia = M.intersectionWithKey err a a'
  ib = M.intersectionWithKey err b b'
  ic = M.intersectionWithKey err c c'
  iAll = M.unions [ia, ib, ic]
  err key _ _ = "Duplicate definition for name '" ++ key ++ "'"

data Quantified a = Q (Set String) a deriving Show

type TypeEnv = Map NTerm (Quantified TyExpr)

type Subst = Map String TyExpr

type FailCont = forall a. String -> TypeCheck a

appFail :: FailCont -> String -> FailCont
appFail fail s = fail . (s ++)

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

union' :: FailCont
  -> TyExpr -> TyExpr -> TypeCheck TyExpr
union' fail ty1 ty2 = do
  ty1' <- evalTy ty1
  ty2' <- evalTy ty2
  union fail ty1' ty2'

evalTy :: TyExpr -> TypeCheck TyExpr
evalTy ty = do
  (TIState _ subst) <- lift get
  return (apply subst ty)

plural :: Int -> String -> String
plural 1 s = s
plural n s = s ++ "s"

union :: FailCont
  -> TyExpr -> TyExpr -> TypeCheck TyExpr
union fail ty1@(TAp tcon1@(NTyCon n1) e1s) 
           ty2@(TAp tcon2@(NTyCon n2) e2s) = if tcon1 == tcon2
  then do
    unionOps <- case strictZipWith (union fail') e1s e2s of
      Right uO -> return uO
      Left (i, j) -> fail' $ "type constructor '" ++ n1 ++ "' expected "
        ++ show i ++ " " ++ plural i "type argument"
        ++ " but was given " ++ show j ++ "."
    argTys <- sequence unionOps
    return (TAp tcon1 argTys)
  else fail' ("expected type constructor " ++ n1 
        ++ " but was given " ++ n2)
  where
  fail' = appFail fail ("can't match expected type " ++ pprintTyExpr ty1
    ++ " with actual type " ++ pprintTyExpr ty2 ++ ", specifically, ")
union fail (TyVar u@(TV flex _)) (TyVar t) = if flex == Rigid
  then varBind fail t (TyVar u)
  else varBind fail u (TyVar t)
union fail (TyVar u) t = varBind fail u t
union fail t (TyVar u) = varBind fail u t
union _ IntTy IntTy = return IntTy
union _ StrTy StrTy = return StrTy
union fail t1 t2 = fail ("can't match expected type " 
  ++ pprintTyExpr t1
  ++ " with actual type " ++ pprintTyExpr t2)

varBind :: FailCont
  -> TyVar -> TyExpr -> TypeCheck TyExpr
varBind fail u@(TV flex name) t = case t of
  TyVar u' | u == u' -> return t
  _ | S.member name (freeVars t) -> fail
      ("occurs check error: unifying type variable '" ++ name ++ 
        "' with type " ++ pprintTyExpr t ++ " would lead to infinite type")
  _ | flex == Rigid -> fail
    ("cannot match rigid type variable '" ++ name ++ "' with type "
        ++ pprintTyExpr t)
  _ -> do
      addSubst name t
      return t

addSubst :: String -> TyExpr -> TypeCheck ()
addSubst u t = do
  TIState i subst <- lift get
  lift $ put (TIState i (M.singleton u t `composeSubst` subst))

freeVarsQ :: Quantified TyExpr -> Set String
freeVarsQ (Q vars tyExpr) = freeVars tyExpr S.\\ vars

freeVars :: TyExpr -> Set String
freeVars ty = case ty of
  TyVar (TV _ a) -> S.singleton a
  TAp dcon exprs -> S.unions (map freeVars exprs)
  _ -> S.empty

instantiate :: Flex -> ((TyExpr -> TyExpr) -> (a -> a)) -> 
  Quantified a -> TypeCheck a
instantiate flex mapf (Q vars expr) = do
  nvars <- mapM (\x -> (,) x <$> newTyVar x flex) (S.toList vars)
  return (mapf (apply (M.fromList nvars)) expr)

instantiateE :: Quantified TyExpr -> TypeCheck TyExpr
instantiateE = instantiate Flex id

instantiateFunc :: Quantified ([TyExpr], TyExpr) -> TypeCheck ([TyExpr], TyExpr)
instantiateFunc = instantiate Flex mapf where
  mapf f (argTys, retTy) = (map f argTys, f retTy)

data TIState = TIState Int Subst deriving Show

newTyVar :: String -> Flex -> TypeCheck TyExpr
newTyVar str flex = do
  (TIState i s) <- lift get
  lift $ put (TIState (i + 1) s)
  return (TyVar (TV flex (str ++ show i)))

type TypeCheck = EitherT String (State TIState)

ti :: FailCont -> Context -> Expr -> TypeCheck TyExpr
ti fail ctxt e = case e of
  EInt _ -> return IntTy
  EStr _ -> return StrTy
  EVar vname -> case M.lookup vname (terms ctxt) of
    Nothing -> fail ("variable '" ++ vname ++ "' not in scope")
    Just scheme -> do
      ty <- instantiateE scheme
      return ty
  EAp inj fname exprs -> case M.lookup fname (funcs ctxt) of
    Nothing -> fail ("function '" ++ fname ++ "' not in scope")
    Just (_, scheme) -> do
      (argTys, retTy) <- instantiateFunc scheme
      argTys2 <- zipWithM (\i -> ti (failF fname i) ctxt) 
         [1 :: Int ..] exprs
      case strictZipWith (unionF fname) (zip [1..] argTys) argTys2 of
        Left (i, j) -> fail ("function " ++ fname ++ " expects " ++ show i
          ++ " arguments, but received " ++ show j)
        Right toUnion -> sequence toUnion
      return retTy
  ELet (TypedIdent (NTerm name) mty) assn expr -> do
    assnTy <- ti (appFail fail ("in let-expression assignment for variable '"
         ++ name ++ "', ")) ctxt $ case mty of
           Nothing -> assn
           Just ty -> Typed assn ty
    ti fail (ctxt { terms = M.insert name (Q S.empty assnTy) (terms ctxt) }) expr
  ECase expr productions -> do
    ety <- ti (appFail fail "in case scrutinee, ") ctxt expr
    (inTys, outTys) <- unzip <$> sequence 
      [ tiCase (appFail fail ("in '" ++ dcon ++  
        "' branch of case expression, ")) ctxt ety d
      | d@(Production (Pattern (NDataCon dcon) _) _) <- productions ]
    outTy <- newTyVar "a" Flex
    _ <- foldM (union' (appFail fail "in case expression, "))
          ety inTys
    resultTy <- foldM (union' (appFail fail "in case expression result, "))
                outTy outTys 
    return resultTy
  Typed e ty -> do
    case kindCheck (tycons ctxt) ty of
      Left err -> fail err
      Right () -> return ()
    ty' <- ti fail ctxt e
    union' fail ty ty'
  where
  unionF fname (i, t1) = union' (failF fname i) t1
  failF fname i = appFail fail ("in argument " ++ show i ++ " of function '"
    ++ fname ++ "', ")

tiCase :: FailCont -> Context -> TyExpr -> Production Expr 
       -> TypeCheck (TyExpr, TyExpr)
tiCase fail ctxt ety 
  (Production (Pattern (NDataCon dcon) argNames) outExpr) = 
    case M.lookup dcon (funcs ctxt) of
      Nothing -> fail ("data constructor " ++ dcon ++
        " not in scope")
      Just (Func, _) -> fail ("expected data constructor" ++
        " in case alternative but found function " ++ dcon)
      Just (DataCon, scheme) -> do
        (argTys, retTy) <- instantiateFunc scheme
        case strictZipWith zipper argNames argTys of
          Left (i, j) -> fail ("data constructor " ++ dcon ++
            " expects " ++ show j ++ " arguments but receives " ++ show i
            ++ " in case alternative")
          Right vs -> do
            outTy <- ti fail (updateCtxt vs) outExpr
            return (retTy, outTy)
  where
  zipper (NTerm x) y = (x, Q S.empty y)
  updateCtxt :: [(String, Quantified TyExpr)] -> Context
  updateCtxt vs = 
    ctxt { terms = (foldr (.) id (map (uncurry M.insert) vs)) (terms ctxt) }

funcCtxt :: Context -> NTerm -> FuncDefn -> Either String Context
funcCtxt ctxt fname@(NTerm fn) fdef = 
  fmap f (elabFunction ctxt fname fdef)
  where
  f :: Quantified ([TyExpr], TyExpr) -> Context
  f ty = Context M.empty (M.singleton fn (Func, ty)) M.empty

elabFunction :: Context -> NTerm -> FuncDefn -> Either String 
  (Quantified ([TyExpr], TyExpr))
elabFunction ctxt fname fdef = result
  where
  (result, _) = runTypeCheck (tiFunc ctxt fname fdef)

generalize :: ([TyExpr], TyExpr) -> Quantified ([TyExpr], TyExpr)
generalize (argTys, retTy) = Q vars (argTys, retTy) where
  vars = S.unions (map freeVars (retTy : argTys))

funcTyVars :: FuncDefn -> Either String (Set String)
funcTyVars (FuncDefn args retTy e) = if S.null freeVarsInE
  then Right vars
  else Left (plural (S.size freeVarsInE) "type variable" ++ " " ++ showV freeVarsInE ++ " not in scope")
  where
  vars = S.unions (map freeVarsM (retTy : map getArgTy args))
  showV = intercalate ", " . map (\x -> "'" ++ x ++ "'") . S.toList
  freeVarsInE = freeTyVars e S.\\ vars
  freeVarsM Nothing = S.empty
  freeVarsM (Just ty) = freeVars ty
  getArgTy (TypedIdent _ mty) = mty
  
duplicated :: Eq a => [a] -> Maybe a
duplicated [] = Nothing
duplicated (x : xs) = (guard (x `elem` xs) >> return x) <|> duplicated xs

createFuncTy :: FailCont -> FuncDefn
  -> TypeCheck (Quantified ([(String, TyExpr)], TyExpr, Expr))
createFuncTy fail fdef@(FuncDefn args retTy e) = do
  case duplicated [ n | TypedIdent (NTerm n) _ <- args ] of
    Nothing -> return ()
    Just x -> fail $ "duplicate function argument '" ++ x ++ "'"
  vars <- case funcTyVars fdef of
    Left e -> fail e
    Right x -> return x
  args' <- mapM (\(TypedIdent (NTerm n) ty) -> 
    do { ty' <- mkTy ty; return (n, ty') }) args
  retTy' <- mkTy retTy
  return (Q vars (args', retTy', e))
  where
  mkTy Nothing = newTyVar "a" Flex
  mkTy (Just e) = return e

freeTyVars :: Expr -> Set String
freeTyVars express = case express of
  EAp _ _ es -> S.unions (map freeTyVars es)
  ECase e prods -> freeTyVars e `S.union` 
    S.unions [ freeTyVars e | Production _ e <- prods ]
  ELet (TypedIdent term (Just ty)) e1 e2 -> freeVars ty `S.union`
    freeTyVars e1 `S.union` freeTyVars e2
  Typed e ty -> freeTyVars e `S.union` freeVars ty
  _ -> S.empty

tiFunc :: Context -> NTerm -> FuncDefn
  -> TypeCheck (Quantified ([TyExpr], TyExpr))
tiFunc ctxt (NTerm fname) fdef@(FuncDefn args _ expr) = do
  funcTy <- createFuncTy fail fdef
  (vs, expRetTy, e) <- instantiate Rigid mapf funcTy
  actRetTy <- ti fail (updateCtxt vs expRetTy) expr
  retTy <- union' fail expRetTy actRetTy
  vs' <- mapM evalTy (map snd vs)
  let actFuncTy = generalize (vs', retTy)
  return actFuncTy
  where
  fail s = hoistEither (Left ("In function declaration '" ++ fname ++
    "', " ++ s))
  mapf f (args, retTy, exp) = (map (second f) args, f retTy
                              , modifyTypesE f exp)
  newArg (TypedIdent (NTerm x) _) = do
    var <- newTyVar "a" Flex
    return (x, var)
  updateCtxt :: [(String, TyExpr)] -> TyExpr -> Context
  updateCtxt vs retTy = 
    ctxt { terms = addTerms (terms ctxt)
         , funcs = M.insert fname fdef (funcs ctxt) }
    where
    addTerms = foldr (.) id $ map (\(x, y) -> M.insert x (Q S.empty y)) vs
    fdef = (Func, Q S.empty (map snd vs, retTy))
         
modifyTypesE :: (TyExpr -> TyExpr) -> Expr -> Expr
modifyTypesE f = g where
  g express = case express of
    EAp inj fun es -> EAp inj fun (map g es)
    ECase e prods -> ECase (g e) [ Production p (g e) 
                                 | Production p e <- prods ]
    ELet (TypedIdent v mty) e1 e2 -> 
      ELet (TypedIdent v (fmap f mty)) (g e1) (g e2)
    Typed e ty -> Typed (g e) (f ty)
    _ -> express


runTypeCheck :: TypeCheck a -> (Either String a, TIState)
runTypeCheck t = runState (runEitherT t) initTIState
  where 
  initTIState = TIState 0 M.empty

subst :: String -> TyExpr -> (TyExpr -> TyExpr)
subst a e ty = case ty of
  TyVar (TV _ a') -> if a == a' then e else ty
  TAp tycon exprs -> TAp tycon (map (subst a e) exprs)
  _ -> ty

