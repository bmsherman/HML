import Compile (compileDecl, printCDecl, CDecl (Label), primOps)
import Parse (parseDecls)
import Primitives (preludeContext, preludeDecls)
import System.IO (stderr, hPutStrLn)

import qualified Data.Map as M

main :: IO ()
main = do
  src <- getContents
  case parseDecls preludeContext src of
    Left e -> hPutStrLn stderr e 
    Right (decls, (ctxt, errors)) -> if null errors
      then do
        let instrs = M.fromList [ (n, is) 
              | Label n is <- concatMap compileDecl preludeDecls ++ primOps 
                           ++ concatMap compileDecl decls ]
        putStrLn "  .global main\n\n  .text\n" 
        sequence_ [ putStrLn (printCDecl (Label n is)) 
          | (n, is) <- M.toList instrs ]
      else mapM_ (hPutStrLn stderr) errors
