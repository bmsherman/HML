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
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import Data.Set (Set)

-- | Values which can be stored in registers
data CVal v = CInt Int32
  | CStr String String
  | CVar v
  deriving Show

-- | "Core" expressions. We are only allowed to apply functions or case
-- analyze things which are already values.
data CExpr v =
    CVal (CVal v)
  | CAp String [CVal v]
  | CCase (CVal v) [Production (CExpr v)]
  | CLet v (CExpr v) (CExpr v)
  deriving Show

-- | Return the set of variable names used in an expression.
varsInExp :: Expr -> Set String
varsInExp e = case e of
  ELet (TypedIdent v _) e1 e2 -> 
    S.insert v (varsInExp e1 `S.union` varsInExp e2)
  ECase e prods -> varsInExp e `S.union` S.fromList
    [ t | Production (Pattern _ terms) _ <- prods, t <- terms ]
  EAp _ _ es -> S.unions (map varsInExp es)
  Typed e _ -> varsInExp e
  _ -> S.empty

-- | Convert a high-level expression to a core expression.
toCExpr :: Set String -> Expr -> CExpr String
toCExpr usedVars e = evalState (f e) (0, usedVars `S.union` varsInExp e) 
  where
  f e = case e of
    EInt i -> return $ CVal (CInt i)
    EStr s -> do
      (i, vs) <- get
      let i' = i + 1
      put (i', vs)
      return $ CVal (CStr (".str" ++ show i') s)
    EVar v -> return $ CVal (CVar v)
    EAp _ func es -> do
      (vs, lets) <- unzip <$> zipWithM funcArgs [1..] es
      case func of 
        "seq" -> head lets <$> f (es !! 1) -- special inlining for seq
        _ -> return $ foldr (.) id lets (CAp func vs)
    ECase e prods -> do
      (e', lett) <- letify "scrut" e
      prods' <- mapM doProd prods
      return $ lett (CCase e' prods')
    ELet (TypedIdent v _) e1 e2 -> CLet v <$> f e1 <*> f e2
    Typed e _ -> f e
  -- introduce a let expression if the expression doesn't involve any
  -- computation
  letify name expr = do
    expr' <- f expr
    case expr' of
      CVal v -> return (v, id)
      x -> do
        v <- newV name
        return (CVar v, CLet v expr')
  funcArgs i = letify ("arg" ++ show i)
  doProd (Production p e) = fmap (Production p) (f e)
  -- create a new variable with a unique name
  newV str = do
    (i, vs) <- get
    let str' : _ = [ n | n <- map (str ++) ( "" : map show [ 0 :: Int .. ] )
                       , not (S.member n vs) ]
    put (i, S.insert str' vs) >> return str'

-- | Either a register or a global label. Used for addresses to call
-- or jump to in the assembly code.
data RegGlob = RGR Register | RGG String deriving Eq

instance Show RegGlob where
  show (RGR reg) = "*" ++ show reg
  show (RGG global) = global

-- Assembly instructions
data Instr = 
    BinOp String Oper Oper
  | UnOp String Oper
  | Call RegGlob
  | Jump String RegGlob
  | Syscall
  | Ret

  | Ascii String
  | Asciz String
  | Quad Int32
  deriving (Eq, Show)

mov = BinOp "mov"
cmp = BinOp "cmp"
movq = BinOp "movq"
clear oper = BinOp "xor" oper oper
add = BinOp "add"
imul = BinOp "imul"
push = UnOp "push"
pop = UnOp "pop"

-- | A labeled set of assembly instructions.
data CDecl = Label String [Instr] deriving (Eq, Show)

-- | X64 registers that we use.
data Register = 
    RAX | RCX | RDX
  | R8 | R9 | R12 | R13
  | RDI | RSI
  | RSP
  deriving (Eq, Enum)

instance Show Register where
  show x = ("%" ++) $ case x of
    RAX -> "rax"
    RCX -> "rcx"
    RDX -> "rdx"
    R8 -> "r8"
    R9 -> "r9"
    R12 -> "r12"
    R13 -> "r13"
    RDI -> "rdi"
    RSI -> "rsi"
    RSP -> "rsp"

-- | An operand for operator instructions such as 'mov'.
data Oper = 
    Global String -- ^ reference to a global label
  | Imm Int32 -- ^ an immediate integer value
  | Reg Register -- ^ contents of a register
  | Mem Int Register -- ^ main memory at a given offset from the address
                     -- in a given register
  deriving (Eq, Show)

printOper :: Oper -> String
printOper o = case o of
  Imm i -> "$" ++ show i
  Global name -> "$" ++ name
  Reg reg -> show reg
  Mem i reg -> (if i == 0 then "" else show i) ++ "(" ++ show reg ++ ")"

printInstr :: Instr -> String
printInstr i = case i of
  BinOp s o1 o2 -> unop s o1 ++ ", " ++ printOper o2
  Call rg -> "call " ++ show rg
  Jump name rg -> name ++ " " ++ show rg
  Syscall -> "syscall"
  UnOp s o -> unop s o
  Ret -> "ret"
  Ascii str -> ".ascii \"" ++ str ++ "\""
  Asciz str -> ".asciz \"" ++ str ++ "\""
  Quad i -> ".quad " ++ show i
  where
  unop str a = str ++ " " ++ printOper a 

printCDecl :: CDecl -> String
printCDecl (Label name instrs) = name ++ ":\n" ++ 
  intercalate "\n" (map (("  " ++) . printInstr) instrs)

-- | Compile the function calls for data constructors.
-- We compile nullary constructors specially, because we shouldn't need to do
-- any allocation each time we call a nullary constructor; we can simply make
-- a global variable that corresponds to the nullary constructor, and return
-- a pointer to that.
constructor :: String -> Int -> [CDecl]
constructor name 0 = 
  [ Label (name ++ ".hash") [Quad (hash name)]
  , Label name [ movq (Global (name ++ ".hash")) (Reg RAX), Ret ]
  ]
constructor name arity = [Label name $ mkFunc $ \xs -> 
  let xs' = take arity xs in
  [ push (Reg x) | x <- reverse xs' ]
  ++ callFunc "malloc" [Imm (8 * (fromIntegral arity + 1))] (\addr ->
  movq (Imm (hash name)) (Mem 0 addr) : 
    [ pop (Mem (8 * i) addr) | i <- take arity [1..] ]
  )]

-- | Use a simple hash so that constructors with different names are given
-- different tags.
hash :: String -> Int32
hash = foldl' (\h c -> 33 * h `B.xor` fromIntegral (fromEnum c)) 5381

-- | Compile a function definition.
func :: String -> FuncDefn -> [CDecl]
func fname (FuncDefn args _ expr) = flip evalState initState $ do
  loads <- zipWithM newVar args' (map Reg funcRegs)
  (decls, instrs) <- ff Nothing fname expr' 
  return (Label fname (concat loads ++ instrs) : decls)
  where
  branchState :: State s a -> State s a
  branchState f = do state <- get; x <- f; put state; return x
  ret :: Maybe (RegGlob, Int) -> [Instr] -> Compile [Instr]
  ret tailc instrs = do
     cleanup <- mkCleanup initStackSize
     return (instrs ++ cleanup ++ [retInstr])
    where
    mkCleanup init = do
      fin <- gets stackSize
      let i = fin - init
      return [ add (Imm (8 * fromIntegral i)) (Reg RSP) | i > 0 ]
    (initStackSize, retInstr) = case tailc of
      Nothing -> (0, Ret)
      Just (retAddr, ss) -> (ss, Jump "jmp" retAddr)
  ff :: Maybe (RegGlob, Int) -> String -> CExpr String  
     -> Compile ([CDecl], [Instr])
  ff tailc lbl e = case e of
    CVal v -> do
      (decls, oper) <- valsProc lbl v
      instrs <- ret tailc [mov oper (Reg RAX)]
      return (decls, instrs)
    CLet v (CVal val) e -> do
      (decls, oper) <- valsProc lbl val
      load <- newVar v oper
      (decls2, instrs2) <- ff tailc lbl e
      return (decls ++ decls2, load ++ instrs2)
    CLet v e1 e2 -> do
      let lblL = lbl ++ ".L"
      ss <- gets stackSize
      let tailc' = Just (RGG lblL, ss)
      (decls, instrs) <- branchState $ ff tailc' (lbl ++ ".l") e1
      load <- newVar v (Reg RAX)
      (decls2, instrs2) <- ff tailc lblL e2
      return (Label lblL (load ++ instrs2) : decls ++ decls2
             , instrs)
    CAp f xs -> do
      moper <- mGetVar f
      let (loadF, rgF) = case moper of
            Nothing -> ([], RGG (toSymbolName f))
            Just oper -> ([movq oper (Reg R12)], RGR R12)
      (decls, argOpers) <- unzip <$> mapM (valsProc lbl) xs
      let loadArgs = zipWith mov argOpers (map Reg funcRegs)
      (,) (concat decls) <$> case tailc of
        Nothing -> ret (Just (rgF, 0)) (loadArgs ++ loadF)
        Just _ -> ret tailc (loadArgs ++ loadF ++ [Call rgF])
    CCase v prods -> do
      (decls1, oper) <- valsProc lbl v
      (decls2, instrs) <- unzip <$> mapM (mkCase tailc lbl) prods
      return (errorMsg : decls1 ++ concat decls2, 
        [ mov oper (Reg RAX) ] ++ concat instrs ++ 
        callFunc "error" [Global errorLbl] (const [])
        )

  valsProc lbl v = case v of
    CInt i -> return ([], Imm i)
    CStr lab str -> let lbl' = lbl ++ lab in return
      ([Label lbl' [Ascii (str ++ "\\0")] ], (Global lbl'))
    CVar var -> (,) [] <$> getVar var
  
  mkCase :: Maybe (RegGlob, Int) -> String -> Production (CExpr String)
         -> Compile ([CDecl], [Instr])
  mkCase tailc lbl (Production (Pattern constr vars) expr) = branchState $ do
    loads <- zipWithM f [1..] vars
    (decls, instrs) <- ff tailc lbl' expr
    return ( Label lbl' (concat loads ++ instrs) : decls ,
      conditionalCall )
    where
    conditionalCall = [ BinOp "cmpl" (Imm (hash constr)) (Mem 0 RAX)
           , Jump "je" (RGG lbl') ]
    lbl' = lbl ++ "." ++ constr
    f i v = newVar v (Mem (8 * i) RAX)

  errorLbl = fname ++ ".err"
  errorMsg = Label errorLbl [ Ascii ("Pattern matching failure in function '"
    ++ fname ++ "'.\\0") ]
  args' = [ n | TypedIdent n _ <- args ]
  expr' = toCExpr (S.fromList args') expr

-- | Primitive integer operations
intOps :: [CDecl]
intOps = negOp : [ Label (toSymbolName x) (intOp y)
  | (x, y) <- [ ("plus", add), ("minus", BinOp "sub"), ("times", imul) ]
  ] where
  intOp op = mkFunc $ \(x:y:_) ->
    [ mov (Reg x) (Reg RAX)
    , op (Reg y) (Reg RAX)
    ]
  negOp = Label (toSymbolName "negate") $ mkFunc $ \(x:_) ->
    [ UnOp "neg" (Reg x), mov (Reg x) (Reg RAX) ]

-- | Primitive integer comparison operations
cmpOps :: [CDecl]
cmpOps = [ Label (toSymbolName (x ++ "Int")) (cmpOp y)
  | (x, y) <- [ ("lt", "l"), ("lte", "le"), ("eq", "e")
              , ("gt", "ge"), ("gte", "g") ]
  ]
  where
  cmpOp trueCond = mkFunc $ \(x:y:_) ->
    [ cmp (Reg y) (Reg x)
    , Jump ("j" ++ trueCond) (RGG "True")
    , mov (Global "False.hash") (Reg RAX) ]

-- | The integer division and modulus operations
divOps :: [CDecl]
divOps = 
  [ Label (toSymbolName n) $ mkFunc $ \(x:y:_) ->
    [ clear (Reg RDX)
    , mov (Reg x) (Reg RAX)
    , mov (Reg y) (Reg RSI)
    , UnOp "idiv" (Reg RSI) ]
    ++ ops
  | (n, ops) <- [("div", []), ("mod", [mov (Reg RDX) (Reg RAX)])] ]

-- | The state of our registers/main memory to keep track of as we walk
-- through the assembly we generate as we compile.
data CompileState = CompileState
  { vars      :: !(Map String Oper)
  , stackSize :: !Int               }

type Compile = State CompileState

-- | Initially, we have no variables, and the stack is empty.
initState :: CompileState
initState = CompileState M.empty 0

-- Introduce a new variable into the local context
newVar :: String -> Oper -> Compile [Instr]
newVar n oldOper = do
  case oldOper of
    Imm _ -> noNewVar; Global _ -> noNewVar; Mem _ RSP -> noNewVar
    _ -> do -- push a new variable onto the stack
      CompileState vars ss <- get
      let ss' = ss + 1
      let oper = Mem 0 RSP
      put (CompileState (M.insert n oper (M.map f vars)) ss')
      return [push oldOper]
  where
  noNewVar = do -- we don't need to put a new variable on the stack
    CompileState vars ss <- get
    put $ CompileState (M.insert n oldOper vars) ss
    return []
  f (Mem i RSP) = Mem (8 + i) RSP
  f x = x

-- | Lookup a variable which should be somewhere in memory.
mGetVar :: String -> Compile (Maybe Oper)
mGetVar n = fmap (M.lookup n . vars) get

-- | If a variable name doesn't exist, then it must be a global
-- (this is only applicable for functions).
getVar :: String -> Compile Oper
getVar n = fromMaybe (Global n) <$> mGetVar n

-- | The registers in which the first 6 arguments of a function are put.
-- Currently, functions with greater than 6 arguments are not supported,
-- and will probably fail to compile. However, you can obviously use ADTs
-- to allow functions to accept more parameters.
funcRegs :: [Register]
funcRegs = [RDI, RSI, RDX, RCX, R8, R9]

mkFunc :: ([Register] -> [Instr]) -> [Instr]
mkFunc f = f funcRegs ++ [Ret]

callFunc :: String -> [Oper] -> (Register -> [Instr]) -> [Instr]
callFunc fname opers andThen = catMaybes (zipWith maybeMov opers funcRegs)
  ++ [Call (RGG fname)] ++ andThen RAX
  where
  maybeMov (Reg r) r' | r == r' = Nothing
  maybeMov o r = Just (mov o (Reg r))

printf :: [Instr]
printf = [ clear (Reg RAX), Call (RGG "printf") ]

intio :: [CDecl]
intio = 
  [ Label "out_int" $ mkFunc $ \(x:_) ->
    [ mov (Reg x) (Reg RSI)
    , mov (Global "int.Format") (Reg RDI) ]
    ++ printf
  , Label "int.Format"
    [ Asciz "%ld" ]
  , Label "in_int" $ mkFunc $ \_ ->
    [ push (Imm 12345678)
    , mov (Reg RSP) (Reg RSI)
    , mov (Global "int.Format") (Reg RDI)
    , clear (Reg RAX)
    , Call (RGG "scanf")
    , pop (Reg RAX)
    ]
  ]

outstring :: CDecl
outstring = Label "out_string" (printf ++ [Ret])

arrayOps :: [CDecl]
arrayOps =
  [ Label "set" $ ($ funcRegs) $ \(arr : pos : val : _) -> 
    offset pos arr ++
    [ mov (Reg val) (Mem 0 arr)
    , Jump "jmp" (RGG "Unit") ]
  , Label "get" $ mkFunc $ \(arr : pos : _) ->
    offset pos arr ++ [ mov (Mem 0 arr) (Reg RAX) ]
  , Label "makeArray" $ mkFunc $ \(size : defVal : _) -> 
    [ push (Reg size)
    , push (Reg defVal)
    , imul (Imm 8) (Reg size) ]
    ++ callFunc "malloc" [Reg size] (\arr -> 
       [pop (Reg R13), pop (Reg R12), push (Reg arr)] ++ 
       callFunc "setAll" [Reg arr, Reg R12, Reg R13] (\_ -> 
         [pop (Reg RAX)]))
  ]
  where
  -- Compute the memory address for arr[pos]
  offset pos arr = 
    [ mov (Reg pos) (Reg R12)
    , imul (Imm 8) (Reg R12)
    , add (Reg R12) (Reg arr) ]


-- | 'error' prints an error message and aborts the computation.
errorDecls :: [CDecl]
errorDecls = [ Label "error" $
  [ mov (Reg RDI) (Reg RSI)
  , mov (Global "error.msg") (Reg RDI)
  ] ++ printf ++ 
  [ mov (Imm 60) (Reg RAX)
  , clear (Reg RDI)
  , Syscall
  ]
  , Label "error.msg" [ Asciz "Error: %s\\n" ]
  ]

-- | The primitive operations.
primOps :: [CDecl]
primOps = outstring : intio ++ intOps ++ divOps
  ++ cmpOps ++ arrayOps ++ errorDecls

-- | Produce the proper x64 for a top-level declaration.
compileDecl :: Decl -> [CDecl]
compileDecl d = case d of
  DataDecl _ (DataDefn _ alts) ->
    [ l | DataAlt name args <- alts, l <- constructor name (length args) ]
  FuncDecl fname funcDefn -> func fname funcDefn

-- | Convert a variable name to a symbol name that's safe to use with
-- the GNU assembler.
toSymbolName :: String -> String
toSymbolName = concatMap f where
  f '\'' = ".P"
  f x = [x]
