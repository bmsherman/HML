module Compile where

import qualified Data.Bits as B
import Data.List (intercalate, foldl')
import Data.Maybe (catMaybes)

data Instr = 
    BinOp String Oper Oper
  | Call String
  | Syscall
  | Push Oper
  | Pop Oper --Register
  | Ret
  | Idiv Oper

  | Asciz String
  deriving (Eq, Show)

mov, movl, xor :: Oper -> Oper -> Instr
mov = BinOp "mov"
movl = BinOp "movl"
xor = BinOp "xor"
add = BinOp "add"
sub = BinOp "sub"
imul = BinOp "imul"

data Decl = Label String [Instr]
  deriving (Eq, Show)

data Register = 
    RAX | RCX | RDX
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | RDI | RSI
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
    EDI -> "edi"

data Oper = Global String
  | Imm Int | Reg Register | Mem Int Register 
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
  Asciz str -> ".asciz \"" ++ str ++ "\""
  where
  unop str a = str ++ " " ++ printOper a 
  binop str a b = unop str a ++ ", " ++ printOper b

printDecl :: Decl -> String
printDecl (Label name instrs) = name ++ ":\n" ++ 
  intercalate "\n" (map (("  " ++) . printInstr) instrs)

mkint :: Decl
mkint = Label "mkint" $ mkFunc $ \(x:_) ->
  [ Push (Reg x) ]
  ++ callFunc "malloc" [Imm 8] (\addr ->
  [ Pop (Mem 0 addr)])

constructor :: String -> Int -> Decl
constructor name arity = Label name $ mkFunc $ \xs -> 
  let xs' = take arity xs in
  [ Push (Reg x) | x <- reverse xs' ]
  ++ callFunc "malloc" [Imm (8 * (arity + 1))] (\addr ->
  movl (Imm (hash name)) (Mem 0 addr) : 
    [ Pop (Mem (8 * i) addr) | i <- take arity [1..] ]
  )
  where
  hash = foldl' (\h c -> 33*h `B.xor` fromEnum c) 5381

intOp :: (Oper -> Oper -> Instr) -> [Instr]
intOp op = mkFunc $ \(x:y:_) ->
  [ mov (Mem 0 x) (Reg RDI)
  , op (Mem 0 y) (Reg RDI)
  ] ++ callFunc "mkint" [Reg RDI] (\_ -> [])

intOps :: [Decl]
intOps = [ Label (toSymbolName x) (intOp y)
  | (x, y) <- [ ("_+", add), ("_-", sub), ("_*", imul) ]
  ]

divOp :: Decl
divOp = Label (toSymbolName "_/") $ mkFunc $ \(x:y:_) ->
  [ xor (Reg RDX) (Reg RDX)
  , mov (Mem 0 x) (Reg RAX)
  , mov (Mem 0 y) (Reg RSI)
  , Idiv (Reg RSI) ]
  ++ callFunc "mkint" [Reg RAX] (\_ -> [])

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

outint :: [Decl]
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

outstring :: Decl
outstring = Label "out_string"
  [ xor (Reg RAX) (Reg RAX)
  , Call "printf"
  , Ret
  ]

prims :: IO ()
prims = mapM_ (putStrLn . printDecl) 
  (constructor "cons" 2 : mkint : outstring : divOp : outint ++ intOps)

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
