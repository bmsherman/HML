module Typecheck where

import AST

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

data Scheme = Scheme (Set String) TyExpr

type TypeEnv = Map NTerm Scheme

mono :: TyExpr -> Scheme
mono = Scheme S.empty

{-
ti :: TypeEnv -> Expr -> Either String Scheme
ti env e = case e of
  EInt _ -> return (mono IntTy)
  EStr _ -> return (mono StrTy)
  EIntBinOp _ e1 e2 -> do
    t1 <- ti env e1
    t2 <- ti env e2
    if t1 == mono IntTy && t2 == mono IntTy
      then return (mono IntTy)
      else Left ""
-}

type Subst = Map String TyExpr

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
g `composeSubst` f = g `M.union` M.map (apply g) f

apply :: Subst -> TyExpr -> TyExpr
apply = M.foldr (.) id . M.mapWithKey subst


strictZipWith :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
strictZipWith f = g where
  g [] [] = Just []
  g [] (_ : _) = Nothing
  g (_ : _) [] = Nothing
  g (x : xs) (y : ys) = fmap (f x y :) (g xs ys)

union :: TyExpr -> TyExpr -> Maybe Subst
union (TAp dcon1 e1s) (TAp dcon2 e2s) = if dcon1 == dcon2
  then do
    substs <- sequence =<< strictZipWith union e1s e2s
    return (foldr composeSubst nullSubst substs)
  else Nothing
union (TyVar u) t = varBind u t
union t (TyVar u) = varBind u t
union IntTy IntTy = return nullSubst
union StrTy StrTy = return nullSubst
union _ _ = Nothing

varBind :: String -> TyExpr -> Maybe Subst
varBind u t = case t of
  TyVar u' | u == u' -> return nullSubst
  _ -> if S.member u (freeVars t)
    then Nothing
    else return (M.singleton u t)


freeVars :: TyExpr -> Set String
freeVars ty = case ty of
  TyVar a -> S.singleton a
  TAp dcon exprs -> S.unions (map freeVars exprs)
  _ -> S.empty

alphaConvert :: (Set String) -> Scheme -> Scheme
alphaConvert set1 (Scheme set2 ty) = 
  (foldr (.) id . map f $ S.toList set2) (Scheme S.empty ty)
  where
  f name (Scheme set thisty) = 
    let name' = uniqueName name (S.union set1 set) in
    Scheme (S.insert name' set) (subst name (TyVar name') thisty)
  
instantiate :: Scheme -> TypeCheck Type
instantiate (Scheme vars expr) = do
  nvars <- mapM (\x -> (,) x <$> newTyVar) vars
  return (apply (M.fromList nvars) t)

data TIState = TIState Int Subst

newTyVar :: TypeCheck Type
newTyVar = do
  (TIState i s) <- get
  put (TIState (i + 1) s)
  return (TyVar ("a" ++ show i))

type TypeCheck = EitherT (State TIState)

ti :: GlobalCtxt -> TypeEnv -> Expr -> Either String (Subst, Type)
ti gctxt env e = case e of
  EInt _ -> return (nullSubst, IntTy)
  EStr _ -> return (nullSubst, StrTy)
  EVar vname -> case M.lookup vname env of
    Nothing -> Left ("Variable " ++ vname ++ " not in scope")
    Just scheme -> do
      ty <- instantiate scheme
      return (nullSubst, ty)
  EConstrAp dcon exprs -> error "haven't done this yet"
  EAp fname exprs -> error "haven't done this yet"
  
      

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
