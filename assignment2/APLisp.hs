module APLisp where

import Text.ParserCombinators.ReadP
import Data.Char(isSpace,isDigit)
import qualified Data.Map as M
import Control.Monad

-- Types and datastructures
data SExp = IntVal Int
          | SymbolVal String
          | ListExp [SExp]
            deriving (Show, Eq, Ord)

type Parser a = ReadP a

type Environment = M.Map String SExp

newtype APLispExec a = RC { runLisp :: Environment -> Either String a }

type Result = Either String SExp

instance Monad APLispExec where
  return x     = RC $ \_ -> Right x
  m >>= f = RC $ \env -> case runLisp m env of
                           Left x -> Left x
                           Right x -> runLisp (f x) env

-- Parser
(<|>) :: ReadP a -> ReadP a -> ReadP a
(<|>) = (+++)

parse :: ReadP a -> ReadS a
parse = readP_to_S

token           :: Parser a -> Parser a
token p          = skipSpaces >> p

symbol :: String -> Parser String
symbol           = token . string

schar :: Char -> Parser Char
schar            = token . char

numberOrSymbol :: Parser SExp
numberOrSymbol = token $ do s <- munch1 $ \c -> not(isSpace c || c `elem` "()")
                            return $ if all isDigit s then IntVal $ read s
                                     else SymbolVal s
sexp :: Parser SExp
sexp = numberOrSymbol
       <|> between (schar '(') (schar ')') sexps
  where sexps = Control.Monad.liftM ListExp (many sexp)

parseString :: String -> Either String SExp
parseString s =
  case parse (do {e <- sexp; token eof; return e}) s of
      [(e, [])] -> Right e
      _         -> Left "Parser Error"

-- Intepreter
true :: SExp -> Bool
true (ListExp []) = False
true _ = True

bindVar :: String -> SExp -> Environment -> Environment
bindVar = M.insert

emptyEnvironment :: Environment
emptyEnvironment = M.empty

local :: (Environment -> Environment) -> APLispExec a -> APLispExec a
local f m = RC $ \env -> let env' = f env
                         in runLisp m env'

ask :: APLispExec Environment
ask = RC $ \env -> Right env

invalid :: String -> APLispExec a
invalid s = RC $ const $ Left s

lookupVar :: String -> APLispExec SExp
lookupVar name = do env <- ask
                    case M.lookup name env of
                      Just c  -> return c
                      Nothing -> invalid "Error: Variable doesn't exist"

evalLambda :: [SExp] -> [SExp] -> SExp -> APLispExec SExp
evalLambda [] [] body = eval body
evalLambda (SymbolVal a:args) (v:vals) body = local (bindVar a v) $
                                            evalLambda args vals body
evalLambda _ _ _ = invalid "Error: Function call failed"

eval :: SExp -> APLispExec SExp
eval (SymbolVal s) = lookupVar s
eval (IntVal v) = return $ IntVal v
eval (ListExp []) = return $ ListExp []
eval (ListExp [SymbolVal "quote", x]) = return x
eval e@(ListExp (SymbolVal "lambda":ListExp _:_)) = return e
eval (ListExp [SymbolVal "if", c, e1, e2]) = do val <- eval c
                                                eval (if true val
                                                      then e1
                                                      else e2)
eval (ListExp [SymbolVal "let", ListExp [], body]) = eval body
eval (ListExp [SymbolVal "let", ListExp
      (ListExp [SymbolVal x,e]:bnds), body]) = do v <- eval e
                                                  local (bindVar x v) $
                                                    eval (ListExp [SymbolVal "let",
                                                          ListExp bnds, body])
eval (ListExp (SymbolVal s:args)) = do eargs <- mapM eval args
                                       apply (SymbolVal s) eargs
eval _ = invalid "Error: Eval failed" -- Catch all wrong (or unhandled) evaluations

apply :: SExp -> [SExp] -> APLispExec SExp
apply (SymbolVal "list") xs = return $ ListExp xs
apply (SymbolVal "+") [IntVal x, IntVal y] = return $ IntVal $ x + y
apply (SymbolVal "/") [IntVal x, IntVal y] = if y == 0
                                             then invalid "Error: Divison by zero"
                                             else return $ IntVal $ x `div` y
apply (SymbolVal "-") [IntVal x, IntVal y] = return $ IntVal $ x - y
apply (SymbolVal "*") [IntVal x, IntVal y] = return $ IntVal $ x * y
apply (SymbolVal "=") [x, y] = return $ ListExp [IntVal 1 | x == y]
apply (SymbolVal "!=") [x, y] = return $ ListExp [IntVal 1 | x /= y]
apply (SymbolVal "<") [IntVal x, IntVal y] = return $ ListExp [IntVal 1 | x < y]
apply (SymbolVal ">") [IntVal x, IntVal y] = return $ ListExp [IntVal 1 | x > y]
apply (SymbolVal "<=") [IntVal x, IntVal y] = return $ ListExp [IntVal 1 | x <= y]
apply (SymbolVal ">=") [IntVal x, IntVal y] = return $ ListExp [IntVal 1 | x >= y]
apply (SymbolVal "cons") [IntVal x, ListExp y] = return $ ListExp $ IntVal x:y
apply (SymbolVal "car") [ListExp (x:_)] = return x
apply (SymbolVal "cdr") [ListExp (_:xs)] = return $ last xs
apply (SymbolVal "funcall")
        [ListExp [SymbolVal "lambda", ListExp [arg], body], IntVal param] =
          evalLambda [arg] [IntVal param] body
apply (SymbolVal "funcall")
        [ListExp [SymbolVal "lambda", ListExp args, body], ListExp params] =
          evalLambda args params body
apply _ _ = invalid "Error: Apply failed" -- Catch all wrong (or unhandled) applications

interpret :: String -> Result
interpret s = case parseString s of
                   Left e -> Left $ show e
                   Right prog ->
                     case interp prog of
                       Left x -> Left x
                       Right cs -> Right cs
  where interp prog = runLisp (eval prog) emptyEnvironment