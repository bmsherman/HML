{-# LANGUAGE Rank2Types #-}
module Typecheck where

import AST

import Control.Applicative ((<$>), (<|>))
import Control.Arrow (second, left)
import Control.Monad (foldM, zipWithM, guard, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, put, get, runState)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

-- | elaborate data declaration (part 1)
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

-- | Elaborate data declaration (part 2)
dataDefCtxt :: Map String Arity -> DataDef 
  -> Either [String] Context
dataDefCtxt tycs (DataDef tyName vars dataAlts) = do
  mLefts  [ left (("in data constructor '" ++ dcon ++ "', ") ++) 
             $ kindCheck tycs' e
             | DataAlt (NDataCon dcon) es <- dataAlts, e <- es ]
  return (Context funcs (M.singleton tyName arity))
  where
  mLefts = go [] where
    go acc [] = if null acc then Right () else Left (reverse acc)
    go acc (x : xs) = case x of
      Left l -> go (l : acc) xs
      Right _ -> go acc xs
  tycs' = M.insert tyName arity tycs
  arity = S.size varSet
  varSet = S.fromList vars
  funcs = M.fromList [ (dcon, Q varSet (TArr x tyExpr))
    | DataAlt (NDataCon dcon) x <- dataAlts ]
  varsE = map (TyVar . TV Flex) vars
  tyExpr = TAp (NTyCon tyName) varsE

type Arity = Int
data Context  = Context
  { terms  :: Map String (Quantified TyExpr)
     -- ^ bound variables and their types (local)
  , tycons :: Map String Arity
     -- ^ type constructors and their kinds (specified entirely by arity)
  }

-- | A totally empty typing context
emptyContext :: Context
emptyContext = Context M.empty M.empty

instance Show Context where
  show = pprintContext

-- | Ensure that type constructors are in scope and that their kinds
-- (arities) are correct
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

-- | pretty-print a typing context
pprintContext :: Context -> String
pprintContext (Context ts tycs) = intercalate "\n" 
  . intercalate [""]
  $ [mp ppTyCons tycs, mp ppTerm ts]
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

-- | Merge two disjoint typing contexts. Throw an error if there are any
-- overlapping names.
unionC :: Context -> Context -> Either [String] Context
unionC (Context a b) (Context a' b') = if M.null iAll
  then Right $ Context (M.union a a') (M.union b b') 
  else Left (M.elems iAll)
  where
  ia = M.intersectionWithKey err a a'
  ib = M.intersectionWithKey err b b'
  iAll = M.unions [ia, ib]
  err key _ _ = "Duplicate definition for name '" ++ key ++ "'"

-- | Intended to put prenex quantifiers on variable and function type
-- expressions.
data Quantified a = Q (Set String) a deriving Show

-- | A map from type variable names to the expressions they correspond to
type Subst = Map String TyExpr

-- | A continuation for failing with a helpful error about "where" we are
type FailCont = forall a. String -> TypeCheck a

-- | Add some description to the failure continuation
appFail :: FailCont -> String -> FailCont
appFail fail s = fail . (s ++)

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
g `composeSubst` f = g `M.union` M.map (apply g) f

-- | Apply a substitution scheme to a type expression
apply :: Subst -> TyExpr -> TyExpr
apply = M.foldr (.) id . M.mapWithKey subst

-- | Zip two lists together, only if they have the same lengths. If they
-- do not, throw an error which provides the two different lengths
strictZipWith :: (a -> b -> c) -> [a] -> [b] -> Either (Int, Int) [c]
strictZipWith f = g (0 :: Int) where
  g _ [] [] = Right []
  g n [] xs@(_ : _) = Left (n, n + length xs)
  g n xs@(_ : _) [] = Left (n + length xs, n)
  g n (x : xs) (y : ys) = fmap (f x y :) (g (n + 1) xs ys)

-- | Unify type expressions, ensuring that all substitutions have already
-- been applied
unify' :: FailCont
  -> TyExpr -> TyExpr -> TypeCheck TyExpr
unify' fail ty1 ty2 = do
  ty1' <- evalTy ty1
  ty2' <- evalTy ty2
  unify fail ty1' ty2'

-- | Apply all of the current substitutions to a type expression
evalTy :: TyExpr -> TypeCheck TyExpr
evalTy ty = do
  (TIState _ subst) <- lift get
  return (apply subst ty)

plural :: Int -> String -> String
plural 1 s = s
plural n s = s ++ "s"

failSubTy :: FailCont -> TyExpr -> TyExpr -> FailCont
failSubTy fail ty1 ty2  = appFail fail ("can't match expected type " 
  ++ pprintTyExpr ty1
  ++ " with actual type " ++ pprintTyExpr ty2 ++ ", specifically, ")

-- | Unify two type expressions
unify :: FailCont
  -> TyExpr -> TyExpr -> TypeCheck TyExpr
unify fail ty1@(TAp tcon1@(NTyCon n1) e1s) 
           ty2@(TAp tcon2@(NTyCon n2) e2s) = if tcon1 == tcon2
  then do
    unifyOps <- case strictZipWith (unify fail') e1s e2s of
      Right uO -> return uO
      Left (i, j) -> fail' $ "type constructor '" ++ n1 ++ "' expected "
        ++ show i ++ " " ++ plural i "type argument"
        ++ " but was given " ++ show j ++ "."
    argTys <- sequence unifyOps
    return (TAp tcon1 argTys)
  else fail' ("expected type constructor " ++ n1 
        ++ " but was given " ++ n2)
  where fail' = failSubTy fail ty1 ty2
unify fail (TyVar u@(TV flex _)) (TyVar t) = if flex == Rigid
  then varBind fail t (TyVar u)
  else varBind fail u (TyVar t)
unify fail ty1@(TArr args1 ret1) ty2@(TArr args2 ret2) = do
  args <- case strictZipWith (unify' fail') args1 args2 of
    Left (i, j) -> fail' ("expected function type " ++ pprintTyExpr ty1 
      ++ " has " ++ show i
      ++ " arguments, but actual function type " ++ pprintTyExpr ty2
      ++ " has " ++ show j)
    Right toUnion -> sequence toUnion
  ret <- unify' fail' ret1 ret2
  return (TArr args ret)
  where fail' = failSubTy fail ty1 ty2
unify fail (TyVar u) t = varBind fail u t
unify fail t (TyVar u) = varBind fail u t
unify _ IntTy IntTy = return IntTy
unify _ StrTy StrTy = return StrTy
unify fail t1 t2 = fail ("can't match expected type " 
  ++ pprintTyExpr t1
  ++ " with actual type " ++ pprintTyExpr t2)

-- | Try binding a type variable to the given type expression
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

-- | Add a substitution to the current map of substitutions
addSubst :: String -> TyExpr -> TypeCheck ()
addSubst u t = do
  TIState i subst <- lift get
  lift $ put (TIState i (M.singleton u t `composeSubst` subst))

-- | Compute all the free variables in a quantified type scheme
freeVarsQ :: Quantified TyExpr -> Set String
freeVarsQ (Q vars tyExpr) = freeVars tyExpr S.\\ vars

-- | Compute all the free variables in a type expression
freeVars :: TyExpr -> Set String
freeVars ty = case ty of
  TyVar (TV _ a) -> S.singleton a
  TAp dcon exprs -> S.unions (map freeVars exprs)
  TArr argTys retTy -> S.unions (map freeVars (retTy : argTys))
  _ -> S.empty

-- | General function for instnatiation a type expression for inference
instantiate :: Flex -> ((TyExpr -> TyExpr) -> (a -> a)) -> 
  Quantified a -> TypeCheck a
instantiate flex mapf (Q vars expr) = do
  nvars <- mapM (\x -> (,) x <$> newTyVar x flex) (S.toList vars)
  return (mapf (apply (M.fromList nvars)) expr)

-- Instantiate the type of a simple expression
instantiateE :: Quantified TyExpr -> TypeCheck TyExpr
instantiateE = instantiate Flex id

-- | State when performing Hindley-Milner type inference. The 'Set String' 
-- holds already-used variables so we don't repeat.  The 'Subst' is
-- a map of type variables to type expressions which must hold.
data TIState = TIState (Set String) Subst deriving Show

-- Create a new type variable
newTyVar :: String -> Flex -> TypeCheck TyExpr
newTyVar str flex = do
  (TIState used s) <- lift get
  let (name : _) = [ n | ext <- "" : map show [ 1 :: Int .. ]
                       , let n = str1 ++ ext, not (S.member n used) ]
  lift (put (TIState (S.insert name used) s))
  return (TyVar (TV flex name))
  where str1 = str

-- | The computational context for performing Hindley-Milner
-- type inference. We can throw an error or we can access some state.
type TypeCheck = ExceptT String (State TIState)

-- | Perform type inference for an expression in a given typing context.
ti :: FailCont -> Context -> Expr -> TypeCheck TyExpr
ti fail ctxt e = case e of
  EInt _ -> return IntTy
  EStr _ -> return StrTy
  EVar vname -> case M.lookup vname (terms ctxt) of
    Nothing -> fail ("variable '" ++ vname ++ "' not in scope")
    Just scheme -> do
      ty <- instantiateE scheme
      return ty
  EAp inj fname exprs -> case M.lookup fname (terms ctxt) of
    Nothing -> fail ("function '" ++ fname ++ "' not in scope")
    Just scheme -> do
      expTyExpr <- instantiateE scheme
      argTys <- zipWithM (\i -> ti (failF fname i) ctxt) 
         [1 :: Int ..] exprs
      retTy <- newTyVar "b" Flex
      TArr argTys' retTy' <- unify fail expTyExpr (TArr argTys retTy)
      return retTy'
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
    _ <- foldM (unify' (appFail fail "in case expression, "))
          ety inTys
    resultTy <- foldM (unify' (appFail fail "in case expression result, "))
                outTy outTys 
    return resultTy
  Typed e ty -> do
    case kindCheck (tycons ctxt) ty of
      Left err -> fail err
      Right () -> return ()
    ty' <- ti fail ctxt e
    unify' fail ty ty'
  where
  unifyF fname (i, t1) = unify' (failF fname i) t1
  failF fname i = appFail fail ("in argument " ++ show i ++ " of function '"
    ++ fname ++ "', ")

-- | Perform type inference for a particular branch of a case expression
-- in a particular typing context
tiCase :: FailCont -> Context -> TyExpr -> Production Expr 
       -> TypeCheck (TyExpr, TyExpr)
tiCase fail ctxt ety 
  (Production (Pattern (NDataCon dcon) argNames) outExpr) = 
    case M.lookup dcon (terms ctxt) of
      Nothing -> fail ("data constructor " ++ dcon ++
        " not in scope")
      Just scheme -> do
        TArr argTys retTy <- instantiateE scheme
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

-- | Typecheck / type-infer a function and produce a typing context
-- to add to the current context.
funcCtxt :: Context -> NTerm -> FuncDefn -> Either String Context
funcCtxt ctxt fname@(NTerm fn) fdef = 
  fmap f (elabFunction ctxt fname fdef)
  where
  toFunc (Q vs (args, ret)) = Q vs (TArr args ret)
  f :: Quantified ([TyExpr], TyExpr) -> Context
  f ty = Context (M.singleton fn (toFunc ty)) M.empty
  elabFunction :: Context -> NTerm -> FuncDefn -> Either String 
    (Quantified ([TyExpr], TyExpr))
  elabFunction ctxt fname fdef = result
    where
    (result, _) = runTypeCheck (tiFunc ctxt fname fdef)

-- | Take the resulting type expression for a function after performing
-- type inference. Remaining free type variables can be universally
-- quantified over.
generalize :: ([TyExpr], TyExpr) -> Quantified ([TyExpr], TyExpr)
generalize (argTys, retTy) = Q vars (argTys, retTy) where
  vars = S.unions (map freeVars (retTy : argTys))

-- | Return the set of type variables quantified over in a user-given
-- function definition.
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
  
-- | If there are no duplicates in the list, then 'Nothing'; otherwise,
-- 'Just' the first duplicate found.
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

-- | The set of free type variables in a term-level expression
freeTyVars :: Expr -> Set String
freeTyVars express = case express of
  EAp _ _ es -> S.unions (map freeTyVars es)
  ECase e prods -> freeTyVars e `S.union` 
    S.unions [ freeTyVars e | Production _ e <- prods ]
  ELet (TypedIdent term (Just ty)) e1 e2 -> freeVars ty `S.union`
    freeTyVars e1 `S.union` freeTyVars e2
  Typed e ty -> freeTyVars e `S.union` freeVars ty
  _ -> S.empty

-- | Type inference for function types
tiFunc :: Context -> NTerm -> FuncDefn
  -> TypeCheck (Quantified ([TyExpr], TyExpr))
tiFunc ctxt (NTerm fname) fdef@(FuncDefn args _ expr) = do
  funcTy <- createFuncTy fail fdef
  (vs, expRetTy, e) <- instantiate Rigid mapf funcTy
  zipWithM_ (\i -> doKindCheck ("in argument " ++ show i ++ ", ") . snd) 
    [1..] vs
  doKindCheck "in return type, " expRetTy
  actRetTy <- ti fail (updateCtxt vs expRetTy) expr
  retTy <- unify' fail expRetTy actRetTy
  vs' <- mapM evalTy (map snd vs)
  let actFuncTy = generalize (vs', retTy)
  return actFuncTy
  where
  doKindCheck failCtxt ty = case kindCheck (tycons ctxt) ty of
    Left e -> (appFail fail failCtxt) e
    Right () -> return ()
  fail s = throwE ("In function declaration '" ++ fname ++ "', " ++ s)
  mapf f (args, retTy, exp) = (map (second f) args, f retTy
                              , modifyTypesE f exp)
  newArg (TypedIdent (NTerm x) _) = (,) x <$> newTyVar "a" Flex
  updateCtxt :: [(String, TyExpr)] -> TyExpr -> Context
  updateCtxt vs retTy = 
    ctxt { terms = M.insert fname fdef (addTerms (terms ctxt)) }
    where
    addTerms = foldr (.) id $ map (\(x, y) -> M.insert x (Q S.empty y)) vs
    fdef = Q S.empty (TArr (map snd vs) retTy)
 
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

-- | Run our typechecking computation
runTypeCheck :: TypeCheck a -> (Either String a, TIState)
runTypeCheck t = runState (runExceptT t) initTIState
  where 
  initTIState = TIState S.empty M.empty

-- | Substitute a type variable name with a given expression
-- in a type expression.
subst :: String -> TyExpr -> (TyExpr -> TyExpr)
subst a e = f where
  f ty = case ty of
    TArr argTys retTy -> TArr (map f argTys) (f retTy)
    TyVar (TV _ a') -> if a == a' then e else ty
    TAp tycon exprs -> TAp tycon (map f exprs)
    _ -> ty

