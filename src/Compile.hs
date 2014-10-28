module Compile where

import Data.List (intercalate)

data Instr = 
    BinOp String Oper Oper
  | Call String
  | Syscall
  | Push Oper
  | Pop Register
  | Ret

  | Asciz String
  deriving (Eq, Show)

mov, movl, xor :: Oper -> Oper -> Instr
mov = BinOp "mov"
movl = BinOp "movl"
xor = BinOp "xor"
add = BinOp "add"
sub = BinOp "sub"
imul = BinOp "imul"
idiv = BinOp "idiv"

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
  Pop r -> "pop " ++ show r
  Ret -> "ret"
  Asciz str -> ".asciz \"" ++ str ++ "\""
  where
  unop str a = str ++ " " ++ printOper a 
  binop str a b = unop str a ++ ", " ++ printOper b

printDecl :: Decl -> String
printDecl (Label name instrs) = name ++ ":\n" ++ 
  intercalate "\n" (map (("  " ++) . printInstr) instrs)

mkint :: Decl
mkint = Label "mkint"
  [ Push (Reg R12)
  , mov (Reg RDI) (Reg R12)
  , mov (Imm 8) (Reg EDI)
  , Call "malloc"
  , movl (Imm 0) (Mem 0 RAX)
  , mov (Reg R12) (Mem 4 RAX)
  , Pop R12
  , Ret
  ]

intOp :: (Oper -> Oper -> Instr) -> [Instr]
intOp op = 
  [ Push (Reg R12)
  , Push (Reg R13)
  , mov (Reg RDI) (Reg R12)
  , mov (Reg RSI) (Reg R13)
  , mov (Mem 4 R12) (Reg RDI)
  , op (Mem 4 R13) (Reg RDI)
  , Call "mkint"
  , Pop R13
  , Pop R12
  , Ret
  ]

intOps :: [Decl]
intOps = [ Label (toSymbolName x) (intOp y)
  | (x, y) <- [ ("_+", add), ("_-", sub), ("_*", imul) ]
  ]

outint :: [Decl]
outint = 
  [ Label "out_int"
    [ mov (Mem 4 RDI) (Reg RSI)
    , mov (Global "intFormat") (Reg RDI)
    , xor (Reg RAX) (Reg RAX)
    , Call "printf"
    , Ret
    ]
  , Label "intFormat"
    [ Asciz "%ld\\n" ]
  ]

prims :: IO ()
prims = mapM_ (putStrLn . printDecl) (mkint : outint ++ intOps)

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
