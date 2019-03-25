import Tokens
import Grammar
import System.Environment
import Control.Exception
import System.IO
import Eval

-- main = alexScanTokens main'

-- main :: IO ()
-- main = catch main' noParse



-- main' = do
--            sourceText <- readFile "test"
--            putStrLn ("Parsing : " ++ sourceText)
--            let parsedProg = setFuncStates (unpackFuncList(lang (alexScanTokens sourceText))) [0,1,2])
--            putStrLn ("Parsed as " ++ (show parsedProg))
-- test = main' [[1,2,3,4,5], [1,2,3,4,5]]


-- main' args = (evalFunc (head funcStates))
--              where currArgs = splitStream args 
--                    funcStates = setFuncStates (unpackFuncList(lang (alexScanTokens "{x, y -> print x + 3*y} -> stdout"))) (head currArgs)

main' = executeProg(unpackFuncList(lang(alexScanTokens "{-> let prev1 = 0; let prev2 = 0} -> {x -> let prev1 = prev1 + prev2 + x; let prev2 = prev1 - prev2 - x; print prev1}"))) [[1,2,3,4,5]]
-- main' = evalExp((ELet "x" (EPlus (Var "x") (EProd (Val 3) (ENext (Var "y") (ELet "x" (EPlus (Var "x") (ENext (Val 1) (EPrint (Var "x"))))))))), [("x", (Val 1)), ("y", (Val 2))], [])
get1 (a,_,_) = a
get2 (_,a,_) = a
get3 (_,_,a) = a



-- noParse :: ErrorCall -> IO ()
-- noParse e = do let err =  show e
--                hPutStr stderr err
--                return ()