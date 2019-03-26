import Tokens
import Grammar
import Eval
import System.Environment
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main' = do
           sourceFile <- getArgs
           input <- getContents
           sourceCode <- readFile (head sourceFile)
           let splitInput = splitStreams input
           let result = executeProg(unpackFuncList(lang(alexScanTokens sourceCode))) splitInput
           putStr (parseOutput result ((length result) `div` (length splitInput)))

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

splitStreams :: String -> [[Int]]
splitStreams streams =  map (map (read::String -> Int)) (map (words) (lines streams))

parseOutput :: [Int] -> Int -> String
parseOutput output coef = unlines (map unwords result)
                          where result = map (map show) (splitList output coef)

splitList :: [Int] -> Int -> [[Int]]
splitList [] coef = []
splitList list coef = [take coef list] ++ (splitList (drop coef list) coef)