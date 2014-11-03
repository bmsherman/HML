module Compile where

import AST

import Control.Applicative ((<$>), (<*>))
import Control.Monad (zipWithM)
import Control.Monad.Trans.State (State, get, put, evalState, gets)

import qualified Data.Bits as B
import Data.Int (Int32)
import Data.List (intercalate, foldl')

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Set (Set)

import Debug.Trace

data CExpr v = CInt Int32
  | CStr String String
  | CVar v
  | CAp String [v]
  | CCase v [Production (CExpr v)]
  | CLet v (CExpr v) (CExpr v)
  deriving Show

varsInExp :: Expr -> Set String
varsInExp e = case e of
  ELet (TypedIdent (NTerm v) _) e1 e2 -> 
    S.insert v (varsInExp e1 `S.union` varsInExp e2)
  ECase e prods -> varsInExp e `S.union` S.fromList
    [ t | Production (Pattern _ terms) _ <- prods, NTerm t <- terms ]
  EAp _ _ es -> S.unions (map varsInExp es)
  Typed e _ -> varsInExp e
  _ -> S.empty

toCExpr :: Set String -> Expr -> CExpr String
toCExpr usedVars e = flip evalState (0, usedVars `S.union` varsInExp e) (f e ) where
  f e = case e of
    EInt i -> return $ CInt i
    EStr s -> do
      (i, vs) <- get
      let i' = i + 1
      put (i', vs)
      return $ CStr ("str" ++ show i') s
    EVar v -> return $ CVar v
    EAp _ f es -> do
      (vs, lets) <- fmap unzip $ zipWithM funcArgs [1..] es
      return $ (foldr (.) id lets) (CAp f vs)
    ECase e prods -> do
      v <- newV "scrut"
      e' <- f e
      prods' <- mapM doProd prods
      return $ CLet v e' (CCase v prods')
    ELet (TypedIdent (NTerm v) _) e1 e2 ->
      CLet v <$> f e1 <*> f e2
    Typed e _ -> f e
  funcArgs i assn = do
    v <- newV ("arg" ++ show i)
    assn' <- f assn
    return $ (v, CLet v assn')
  doProd (Production p e) = fmap (Production p) (f e)
  newV str = do
    (i, vs) <- get
    if S.member str vs then newV (str ++ "'") else do
      put (i, S.insert str vs)
      return str

data Instr = 
    BinOp String Oper Oper
  | Call String
  | Syscall
  | Push Oper
  | Pop Oper --Register
  | Ret
  | Idiv Oper

  | Ascii String
  | Asciz String
  deriving (Eq, Show)

mov, movl, xor :: Oper -> Oper -> Instr
mov = BinOp "mov"
movl = BinOp "movl"
movq = BinOp "movq"
xor = BinOp "xor"
add = BinOp "add"
sub = BinOp "sub"
imul = BinOp "imul"

data CDecl = Label String [Instr]
  deriving (Eq, Show)

data Register = 
    RAX | RCX | RDX
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | RDI | RSI
  | RSP
  | EDI
  deriving (Eq, Enum)

instance Show Register where
  show x = ("%" ++) $ case x of
    RAX -> "rax"
    RCX -> "rcx"
    RDX -> "rdx"
    R8 -> "r8"
    R9 -> "r9"
    R10 -> "r10"
    R11 -> "r11"
    R12 -> "r12"
    R13 -> "r13"
    R14 -> "r14"
    R15 -> "r15"
    RDI -> "rdi"
    RSI -> "rsi"
    RSP -> "rsp"
    EDI -> "edi"

data Oper = Global String
  | Imm Int32 | Reg Register | Mem Int Register 
  deriving (Eq, Show)

printOper :: Oper -> String
printOper o = case o of
  Imm i -> "$" ++ show i
  Global name -> "$" ++ name
  Reg reg -> show reg
  Mem i reg -> (if i == 0 then "" else show i) ++ "(" ++ show reg ++ ")"

printInstr :: Instr -> String
printInstr i = case i of
  BinOp s o1 o2 -> binop s o1 o2
  Call s -> "call " ++ s
  Syscall -> "syscall"
  Push o -> unop "push" o
  Idiv o -> unop "idiv" o
  Pop o -> unop "pop" o
  Ret -> "ret"
  Ascii str -> ".ascii \"" ++ str ++ "\""
  Asciz str -> ".asciz \"" ++ str ++ "\""
  where
  unop str a = str ++ " " ++ printOper a 
  binop str a b = unop str a ++ ", " ++ printOper b

printCDecl :: CDecl -> String
printCDecl (Label name instrs) = name ++ ":\n" ++ 
  intercalate "\n" (map (("  " ++) . printInstr) instrs)

mkint :: CDecl
mkint = Label "mkint" $ mkFunc $ \(x:_) ->
  [ Push (Reg x) ]
  ++ callFunc "malloc" [Imm 8] (\addr ->
  [ Pop (Mem 0 addr)])

constructor :: String -> Int -> CDecl
constructor name arity = Label name $ mkFunc $ \xs -> 
  let xs' = take arity xs in
  [ Push (Reg x) | x <- reverse xs' ]
  ++ callFunc "malloc" [Imm (8 * (fromIntegral arity + 1))] (\addr ->
  movl (Imm (hash name)) (Mem 0 addr) : 
    [ Pop (Mem (8 * i) addr) | i <- take arity [1..] ]
  )
  where
  hash :: String -> Int32
  hash = foldl' (\h c -> 33*h `B.xor` fromIntegral (fromEnum c)) 5381

func :: String -> FuncDefn -> [CDecl]
func fname (FuncDefn args _ expr) = flip evalState initState $ do
  --traceShow expr' True `seq` return ()
  loads <- zipWithM newVar args' (map Reg funcRegs)
  (decls, instrs) <- ff expr' 
  cleanup <- mkCleanup
  return (Label fname (loads ++ instrs ++ cleanup ++ [Ret]) : decls)
  where
  ff e = case e of
    CInt i -> return $ ([], callFunc "mkint" [Imm i] (\_ -> []))
    CStr lab str -> return ([Label lab [Ascii (str ++ "\\0")]]
      , [mov (Global lab) (Reg RAX)])
    CVar v -> do
      oper <- getVar v
      return ([], [mov oper (Reg RAX)])
    CLet v e1 e2 -> do
      (decls, instrs) <- ff e1
      load <- newVar v (Reg RAX)
      (decls2, instrs2) <- ff e2
      return (decls ++ decls2, instrs ++ [load] ++ instrs2)
    CAp f xs -> do
      d <- funcCall (toSymbolName f) xs (\_ -> return [])
      return ([], d)
    
  args' = [ n | TypedIdent (NTerm n) _ <- args ]
  expr' = toCExpr (S.fromList args') expr

intOp :: (Oper -> Oper -> Instr) -> [Instr]
intOp op = mkFunc $ \(x:y:_) ->
  [ mov (Mem 0 x) (Reg RDI)
  , op (Mem 0 y) (Reg RDI)
  ] ++ callFunc "mkint" [Reg RDI] (\_ -> [])

intOps :: [CDecl]
intOps = [ Label (toSymbolName x) (intOp y)
  | (x, y) <- [ ("_+", add), ("_-", sub), ("_*", imul) ]
  ]

divOp :: CDecl
divOp = Label (toSymbolName "_/") $ mkFunc $ \(x:y:_) ->
  [ xor (Reg RDX) (Reg RDX)
  , mov (Mem 0 x) (Reg RAX)
  , mov (Mem 0 y) (Reg RSI)
  , Idiv (Reg RSI) ]
  ++ callFunc "mkint" [Reg RAX] (\_ -> [])

data CompileState = CompileState
  { vars :: !(Map String Oper)
  , stackSize :: !Int
  , freeRegs :: [Register]
  }

mkCleanup :: Compile [Instr]
mkCleanup = do
  i <- gets stackSize
  return $ if i > 0
    then [add (Imm (8 * fromIntegral i)) (Reg RSP)]
    else []

initState :: CompileState
initState = CompileState M.empty 0 tempRegs

newVar :: String -> Oper -> Compile Instr
newVar n oldOper = do
  CompileState vars stackSize freeRegs <- get
  case freeRegs of
    (r : regs) -> do
      let oper = Reg r
      put (CompileState (M.insert n oper vars) stackSize regs)
      return (mov oldOper oper)
    [] -> do
      let stackSize' = stackSize + 1
      let oper = Mem 0 RSP
      put (CompileState (M.insert n oper (M.map f vars)) stackSize' freeRegs)
      return (Push oldOper)
  where
  f (Mem i RSP) = Mem (8 + i) RSP
  f x = x

getVar :: String -> Compile Oper
getVar n = fmap ((M.! n) . vars) get
    

type Compile = State CompileState

funcRegs :: [Register]
funcRegs = [RDI, RSI, RDX, RCX, R8, R9]

tempRegs :: [Register]
tempRegs = [R12, R13, R14, R15]

mkFunc :: ([Register] -> [Instr]) -> [Instr]
mkFunc f = f funcRegs ++ [Ret]

callFunc :: String -> [Oper] -> (Register -> [Instr]) -> [Instr]
callFunc fname opers andThen = catMaybes (zipWith maybeMov opers funcRegs)
  ++ [Call fname] ++ andThen RAX
  where
  maybeMov (Reg r) r' | r == r' = Nothing
  maybeMov o r = Just (mov o (Reg r))

funcCall :: String -> [String] -> (Register -> Compile [Instr])
  -> Compile [Instr]
funcCall fname args andThen = do
  opers <- mapM getVar args
  ((zipWith mov opers (map Reg funcRegs) ++ [Call fname]) ++) <$> andThen RAX

outint :: [CDecl]
outint = 
  [ Label "out_int" $ mkFunc $ \(x:_) ->
    [ mov (Mem 0 x) (Reg RSI)
    , mov (Global "intFormat") (Reg RDI)
    , xor (Reg RAX) (Reg RAX)
    , Call "printf"
    , Ret
    ]
  , Label "intFormat"
    [ Asciz "%ld\\n" ]
  ]

outstring :: CDecl
outstring = Label "out_string"
  [ xor (Reg RAX) (Reg RAX)
  , Call "printf"
  , Ret
  ]

errorDecls :: [CDecl]
errorDecls = [ Label "_error"
  [ xor (Reg RAX) (Reg RAX)
  , mov (Reg RDI) (Reg RSI)
  , mov (Global "_error.msg") (Reg RDI)
  , Call "printf"
  , mov (Imm 60) (Reg RAX)
  , xor (Reg RDI) (Reg RDI)
  , Syscall
  ]
  , Label "_error.msg" [ Asciz "Error: %s\\n" ]
  ]

prims :: IO ()
prims = mapM_ (putStrLn . printCDecl) 
  (
  constructor "cons" 2 : mkint : outstring : divOp : outint ++ intOps
  ++ 
  func "main" (FuncDefn [] Nothing 
    (EAp Func "seq"
      [ EAp Func "out_string" [EStr "blahblah" ] ,
    (EAp Func "seq" 
      [ EAp Func "out_int" [EAp Func "_+" [EInt 1, EInt 2]]
      , EAp Func "out_string" [EStr "Hello, world!\\n"] 
      ] 
    )
    ]
    )
  )
  ++ func "seq" (FuncDefn 
    [TypedIdent (NTerm "x") Nothing
    , TypedIdent (NTerm "y") Nothing] Nothing (EVar "y"))
  ++ errorDecls
  )

toSymbolName :: String -> String
toSymbolName = concatMap f where
  f '\'' = ".P"
  f '>' = ".gt"
  f '<' = ".lt"
  f '=' = ".eq"
  f '+' = ".plus"
  f '-' = ".minus"
  f '~' = ".negate"
  f '*' = ".times"
  f '/' = ".div"
  f x = [x]
