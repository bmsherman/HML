module Primitives where

import AST (Decl)
import Paths_HW4 (getDataFileName)
import Parse (parseDecls)
import Typecheck (Context, emptyContext)

import System.IO.Unsafe (unsafePerformIO)

-- | A typing context that includes built-in functions from a Prelude
-- code file
preludeContext :: Context
preludeDecls :: [Decl]
(preludeContext, preludeDecls) = (ctxt, decls)
  where
  preludeSrc = unsafePerformIO $ 
    readFile =<< getDataFileName "Prelude.hm"
  Right (decls, (ctxt, _)) = parseDecls emptyContext preludeSrc

