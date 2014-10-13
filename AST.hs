module AST where

import Data.List (intercalate)

newtype NTerm = NTerm String deriving (Eq, Show, Ord)
newtype NTyCon = NTyCon String deriving (Eq, Show, Ord)
newtype NDataCon = NDataCon String deriving (Eq, Show, Ord)

--data TyIdent = TyLit NTyCon | TyVar String deriving Show

data IntBinOp = Plus | Minus | Times | Div deriving Show
data IntBinCmp = CmpLT | CmpLEQ | CmpEQ deriving Show

data Express e = I | S | P e e

data Positioned = WP Pos (Express Positioned)

data Pos = Pos

data Expr = EInt Int
  | EStr String
  | EVar String
  | EConstrAp NDataCon [Expr]
  | EAp NTerm [Expr]
  | ECase Expr [Production Expr]
  | ELet TypedIdent Expr Expr
  | EIntBinOp IntBinOp Expr Expr
  | EIntBinCmp IntBinCmp Expr Expr
  | ENegate Expr
  | ESeq Expr Expr
  | Typed Expr TyExpr
  deriving Show

data CExpr = CLit Lit
  | CVar NTerm
  | CAp String [NTerm]
  | CCase NTerm [Production CExpr]
  | CLet NTerm CExpr CExpr

{-
toCore :: Expr -> CExpr
toCore e = case e of
  EInt i -> CLit (CInt i)
  EStr s -> CLit (CStr s)
  EConstrAp (NDataCon dcon) exprs -> CAp dcon (map toCore exprs)
  EAp (Nterm f) exprs -> CAp f (map toCore exprs)
  ECase scrut prods -> CLet
-}

data Lit = CInt Int | CStr String

data TyExpr = IntTy | StrTy
  | TyVar String
  | TAp NTyCon [TyExpr]
  deriving Show

pprintTyExpr :: TyExpr -> String
pprintTyExpr x = case x of
  IntTy -> "Int"
  StrTy -> "String"
  TyVar s -> s
  TAp (NTyCon tycon) exprs -> tycon ++ "(" 
    ++ intercalate ", " (map pprintTyExpr exprs) ++ ")"

data DataDefn = DataDefn [NTerm] [DataAlt]
  deriving Show

data DataAlt = DataAlt NDataCon [TyExpr]
  deriving Show

data FuncDefn = FuncDefn [TypedIdent] (Maybe TyExpr) Expr
  deriving Show

data TypedIdent = TypedIdent NTerm (Maybe TyExpr)
  deriving Show

data Decl = DataDecl NTyCon DataDefn | FuncDecl NTerm FuncDefn
  deriving Show

data Production e = Production Pattern e
  deriving Show

data Pattern = Pattern NDataCon [NTerm]
  deriving Show
