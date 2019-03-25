module Eval where
import Grammar

type Env = [(String, Exp)]
data Frame = Add Exp Env | AddE Exp
           | Subtr Exp Env | SubtrE Exp
           | Mult Exp Env | MultE Exp
           | Div Exp Env | DivE Exp
           | Mod Exp Env | ModE Exp
           | Pow Exp Env | PowE Exp
           | Neg Env
           | Let String Env
           | Print
           | PrintE Int
           | Next Exp
           deriving (Show, Eq)
type Kont = [ Frame ]
type State = (Exp, Env, Kont)

unpackFuncList:: FuncList -> [FuncDecl]
unpackFuncList (FuncList f FEmpty) = [f]
unpackFuncList (FuncList f fl) = [f] ++ unpackFuncList fl

unpackVarList :: VarList -> [Int] -> Env -> Env
unpackVarList (VEmpty) ss env = env
unpackVarList (VarList n v) (s:ss) env = unpackVarList v ss (update env n (Val s))

splitStream:: [[Int]] -> [[Int]]
splitStream ls | null (head ls) = [] 
splitStream ls = [(map head ls)] ++ splitStream(map (drop 1) ls)


executeFunc :: FuncDecl -> State -> [[Int]] -> [Int]
executeFunc fDec s [] = []
executeFunc (FuncDecl v e) (exp, env, k) (i:is) = (getOutputs state) ++ (executeFunc (FuncDecl v e) (flushEnv state) is)
                                                  where state = evalExp (e, (unpackVarList v i env), k)

getOutputs :: State -> [Int]
getOutputs (e,env,k) = map (value.snd) (filter (\(n, (Val v)) -> n == "output") env)

flushEnv :: State -> State
flushEnv (e,env,k)  = (e, (filter (\(n, e) -> n /= "output") env), k)

executePreprocessor :: FuncDecl -> Env -> State
executePreprocessor (FuncDecl (VEmpty) e) env = evalExp (e, env, [])

setEnvironment :: [FuncDecl] -> State
setEnvironment [] = ((Val 0), [], [])
setEnvironment [fd] = executePreprocessor fd []
setEnvironment (fd:fds) = executePreprocessor fd (getEnv(setEnvironment fds))

executeProg :: [FuncDecl] -> [[Int]] -> [Int]
executeProg fds input = executeFunc func (setEnvironment preprocessors) inputs
                         where preprocessors = filter (\(FuncDecl v e) -> v == (VEmpty)) fds
                               func = head (filter (\(FuncDecl v e) -> v /= (VEmpty)) fds)
                               inputs = splitStream input

getEnv :: State -> Env
getEnv (e,env, k) = env

-- getStreamVars :: VarList -> [String]
-- getStreamVars (VEmpty) = []
-- getStreamVars (VarList n v) = [n] ++ (getStreamVars v)


-- setFuncState:: FuncDecl -> [Int] -> (State, [Exp], [String], [[Int]])
-- setFuncState (FuncDecl v e) input = (((Val 0), (unpackVarList v input), []), es, (getStreamVars v), [])
--                                      where es = unpackExpList e

-- setFuncStates:: [FuncDecl] -> [Int] -> FuncStates
-- setFuncStates [] input = []
-- setFuncStates (fd:fds) input = [(setFuncState fd input)] ++ (setFuncStates fds input)

update :: Env -> String -> Exp -> Env
update env n e = [(n,e)] ++ [el | el <- env, fst(el)/=n]

isValue :: Exp -> Bool
isValue (Val _) = True
isValue _ = False

value :: Exp -> Int
value (Val a) = a

getValue:: String -> Env -> Exp
getValue x [] = error "Varible not found"
getValue x ((y,e):env) | x == y = e
                       | otherwise = getValue x env

eval:: State -> State

eval ((Var n), env, k) = ((getValue n env), env, k)

eval (val, env, []) | isValue val = (val, env, [])

-- eval power
eval ((EPow e1 e2), env, k) = (e1, env, (Pow e2 env):k)
eval ((Val n), env1, (Pow e env2):k) = (e, env2, (PowE (Val n)):k)
eval ((Val m), env, (PowE (Val n)):k) = (Val (n ^ m), env, k)

-- eval mod
eval ((EMod e1 e2), env, k) = (e1, env, (Mod e2 env):k)
eval ((Val n), env1, (Mod e env2):k) = (e, env2, (ModE (Val n)):k)
eval ((Val m), env, (ModE (Val n)):k) = (Val (n `mod` m), env, k)

-- eval div
eval ((EDiv e1 e2), env, k) = (e1, env, (Div e2 env):k)
eval ((Val n), env1, (Div e env2):k) = (e, env2, (DivE (Val n)):k)
eval ((Val m), env, (DivE (Val n)):k) = (Val (n `div` m), env, k)

-- eval prod
eval ((EProd e1 e2), env, k) = (e1, env, (Mult e2 env):k)
eval ((Val n), env1, (Mult e env2):k) = (e, env2, (MultE (Val n)):k)
eval ((Val m), env, (MultE (Val n)):k) = (Val (n * m), env, k)

-- eval plus
eval ((EPlus e1 e2), env, k) = (e1, env, (Add e2 env):k)
eval ((Val n), env1, (Add e env2):k) = (e, env2, (AddE (Val n)):k)
eval ((Val m), env, (AddE (Val n)):k) = (Val (n + m), env, k)

-- eval minus
eval ((EMinus e1 e2), env, k) = (e1, env, (Subtr e2 env):k)
eval ((Val n), env1, (Subtr e env2):k) = (e, env2, (SubtrE (Val n)):k)
eval ((Val m), env, (SubtrE (Val n)):k) = (Val (n - m), env, k)

-- eval negation
eval ((ENeg e1), env, k) = (e1, env, (Neg env) : k)
eval ((Val n), env1, (Neg env2):k) = (Val (negate n), env1, k)

-- eval let
eval ((ELet x exp), env, k) = (exp, env, (Let x env):k)
eval (exp, env1, (Let x env2):k) | isValue exp = (exp, update env2 x exp, k)

eval ((EPrint exp), env,k) = (exp, env, (Print):k)
eval ((Val n), env, (Print):k) = ((Val n), env, (PrintE n):k)

eval ((ENext exp1 exp2), env, k) = (exp1, env, (k ++ [(Next exp2)]))
eval ((Val n), env, (Next exp): k) = (exp, env, k)

eval (e, env, (PrintE n):k) = (e, env, (PrintE n):k)

-- eval error
eval (exp, env, k) = error "Evaluation error"


evalExp:: State -> State
evalExp ((Val n), env, []) = ((Val n), env, [])
evalExp (e, env, (PrintE n):k) = evalExp (e, ("output",(Val n)):env, k)
evalExp s = evalExp(eval s)

-- evalFunc:: (State, [Exp], [String], [[Int]]) -> (State, [Exp], [String], [[Int]])
-- evalFunc ((e,env,k), [], v, out) = ((e,env,k), [], v, out)
-- evalFunc ((e1,env,k),(e2:es), vs, out) = evalFunc ((evalExp (e2,env,k)), es, vs, out)

-- printValues:: Env -> [Exp] -> [Int]
-- printValues env vs = map (value.snd) [e | e <- env, (Var v) <- vs, fst(e)==v]

-- -- setOutput :: Env -> [Int]
-- setOutput env = do output <- filter (\(n,v) -> n=="output") env
--                    env <- filter (\(n,v) -> n/="output") env
--                    return output


-- updateStreamVars :: State -> [Int] -> [String] -> State
-- updateStreamVars (e,env,k) [] [] = (e,env,k)
-- updateStreamVars (e,env,k) (val:values) (var:vars) = updateStreamVars (e,(update env var (Val val)),k) values vars

-- evalStream :: State -> (State, [Exp], [String]) -> [[Int]] -> State
-- evalStream s1 (s2,es,var) [] = s1
-- evalStream s1 (s2,es,var) (l:ls) = evalStream state (state, es, var) ls
--                                    where state = (evalFunc ((updateStreamVars s1 l var),es,var))


-- getEnv (e,env,k) = env

-- -- evalStream fd vs [] = []
-- -- evalStream fd vs s:ss = (evalFunc fd vs s) ++ (evalStream fd ss)





