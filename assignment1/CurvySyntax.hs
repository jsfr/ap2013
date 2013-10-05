module CurvySyntax where

-- Imports
import Data.Char
import SimpleParse
import CurveAST

-- Types
type Error = String

-- Parsers
number :: Parser Number
number = n0 <++ n1
  where n0 = do i <- integer
                string "."
                f <- integer
                return $ read $ i ++ "." ++ f
        n1 = do i <- integer
                return $ read i
        integer = many1 $ satisfy isDigit


ident :: Parser Ident
ident = do spaces
           s <- many1 (satisfy validChar)
           if s `notElem` keywords
             then return s
             else reject
  where validChar c = any (\m -> m c) [isDigit, isUpper, isLower, (==) '_']
        keywords = ["where", "refv", "refh", "rot", "width", "height"]

parens :: Parser a -> Parser a
parens p = do symbol "("
              x <- p
              symbol ")"
              return x

expr :: Parser Expr
expr = expr0
  where expr0 = expr1 `chainl1` plusop
        expr1 = expr2 `chainl1` multop
        expr2 = do spaces
                   n <- number
                   return $ Const n
                <|> parens expr
                <|> width
                <|> height
        width = symbol "width" >> do c <- curve
                                     return $ Width c
        height = symbol "height" >> do c <- curve
                                       return $ Height c
        plusop = symbol "+" >> return Add
        multop = symbol "*" >> return Mult

point :: Parser Point
point = parens (do e1 <- expr
                   symbol ","
                   e2 <- expr
                   return $ Point e1 e2)

curve :: Parser Curve
curve = curve0 `chainl1` connop
  where curve0 = curve1 `chainl1` overop
        curve1 = eval curve2 "->" Translate point many
        curve2 = eval curve3 "**" Scale expr many
        curve3 = eval curve4 "refv" Refv expr many1
        curve4 = eval curve5 "refh" Refh expr many1
        curve5 = eval curve6 "rot" Rot expr many1
        curve6 = do p <- point
                    return $ Single p
                 <|>
                 do i <- ident
                    return $ Id i
                 <|> parens curve
        connop = symbol "++" >> return Connect
        overop = symbol "^" >> return Over
        eval c str dat p1 p2 = do x <- c
                                  p2 space
                                  string str
                                  p2 space
                                  y <- p1
                                  return $ dat x y
                               <|> c

def :: Parser Def
def = def0 <|> def1
  where def0 = do i <- ident
                  symbol "="
                  c <- curve
                  return $ Def i c []
        def1 = do (Def i c _) <- def0
                  many1 space
                  symbol "where"
                  symbol "{"
                  many1 space
                  ds <- many1 def
                  symbol "}"
                  return $ Def i c ds

program :: Parser Program
program = many1 def

-- Input functions
parseString :: String -> Either Error Program
parseString s = case prog of
                []  -> Left "Error: Parsing failed!"
                p:_ -> Right p
  where prog = parse' (do vs <- program
                          token eof
                          return vs) s

parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename