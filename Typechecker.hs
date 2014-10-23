import Parse (parseDecls)
import Primitives (preludeContext)
import Typecheck (Context)

main :: IO ()
main = do
  src <- getContents
  case parseDecls preludeContext src of
    Left e -> putStrLn e 
    Right (decls, (ctxt, errors)) -> do
      mapM_ putStrLn errors
      putStrLn "\nTypes:"
      print ctxt 
