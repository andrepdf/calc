module Parser
    ( State(..), Error(..), Result(..), Parser(..)
    , diagnose, try, choice, chainl1
    , (<?>), (<:>), (<++>)
    , void, satisfy, char, string, space, spaces, digit, alpha, alphanum
    , natural, integer, decimal, identifier
    , value, atom, term, letExpr, expression, parse )
where

import Data.Char           ( isSpace, isDigit, isAlpha, isAscii )
import Control.Applicative ( Alternative(..) )

import Syntax ( Expr(..) )

--- Types ---

data State = State
    { stateStr :: String
    , stateIdx :: Int }
    deriving Show

data Error = Error
    { errorMsg :: String
    , errorIdx :: Int }
    deriving Show

data Result a
    = Failure Error
    | Success a State
    deriving Show

newtype Parser a = Parser
    { runParser :: State -> Result a }

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f px = Parser $ \s0 ->
        case runParser px s0 of
            Failure err  -> Failure err
            Success x s1 -> Success (f x) s1

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = Parser $ \s0 -> Success x s0
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = Parser $ \s0 ->
        case runParser pf s0 of
            Failure err  -> Failure err
            Success f s1 -> runParser (fmap f px) s1

instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser $ \s0 -> Failure $ Error "empty alternative" (stateIdx s0)
    -- (<|>) :: Parser a -> Parser a -> Parser a
    px <|> py = Parser $ \s0 ->
        case runParser px s0 of
            Failure err  ->
                if stateIdx s0 == errorIdx err
                    then runParser py s0
                    else Failure err
            Success x s1 -> Success x s1

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    px >>= f = Parser $ \s0 ->
        case runParser px s0 of
            Failure err  -> Failure err
            Success x s1 -> runParser (f x) s1

--- Utility ---

diagnose :: State -> Error -> String
diagnose (State str _) (Error msg idx) =
    let ptr = replicate idx ' ' ++ "^" in
    "| Syntax Error: " ++ msg ++ "\n|   " ++ str ++ "\n|   " ++ ptr

try :: Parser a -> Parser a
try px = Parser $ \s0 ->
    case runParser px s0 of
        Failure err  -> Failure $ Error (errorMsg err) (stateIdx s0)
        Success x s1 -> Success x s1

choice :: String -> [Parser a] -> Parser a
choice msg = foldr (<|>) (empty <?> msg)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 px pf = px >>= fold
    where
        fold x = choice "placeholder"
            [ pf >>= \f -> px >>= \y -> fold (f x y)
            , pure x ]

infix 0 <?>
(<?>) :: Parser a -> String -> Parser a
px <?> msg = Parser $ \s0 ->
    case runParser px s0 of
        Failure err  -> Failure $ Error msg (errorIdx err)
        Success x s1 -> Success x s1

infixr 5 <:>
(<:>) :: Parser a -> Parser [a] -> Parser [a]
(<:>) = liftA2 (:)

infixr 5 <++>
(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
(<++>) = liftA2 (++)

--- Basic Parsers ---

void :: Parser ()
void = Parser $ \s0 ->
    case stateStr s0 of
        [] -> Success () s0
        _  -> Failure $ Error "expecting end of input" (stateIdx s0)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s0 ->
    let idx = stateIdx s0 in
    case stateStr s0 of
        []           -> Failure $ Error "unexpected end of input" idx
        x : xs | f x -> Success x $ State xs (idx + 1)
        _            -> Failure $ Error "unexpected character" idx

char :: Char -> Parser Char
char c = satisfy (== c)
    <?> "expecting character '" ++ [c] ++ "'"

string :: String -> Parser String
string s = foldr (<:>) (pure []) (map char s)
    <?> "expecting string '" ++ s ++ "'"

space :: Parser Char
space = satisfy isSpace
    <?> "expecting space"

spaces :: Parser ()
spaces = () <$ many space

digit :: Parser Char
digit = satisfy isDigit
    <?> "expecting digit"

alpha :: Parser Char
alpha = satisfy (\c -> isAlpha c && isAscii c)
    <?> "expecting letter"

alphanum :: Parser Char
alphanum = choice "expecting alphanumberic"
    [ alpha
    , digit ]

natural :: Parser String
natural = some digit
    <?> "expecting natural"

integer :: Parser String
integer = choice "expecting integer"
    [ char '-' <:> natural
    , natural ]

decimal :: Parser String
decimal = choice "expecting decimal"
    [ try $ integer <++> (char '.' <:> natural)
    , integer ]

identifier :: Parser String
identifier = alpha <:> many alphanum
    <?> "expecting identifier"

--- Expression Parsers ---

value :: Parser Expr
value = Val . read <$> decimal
    <?> "expecting expression"

variable :: Parser Expr
variable = Var <$> identifier
    <?> "expecting variable"

parentheses :: Parser Expr -> Parser Expr
parentheses px = char '(' *> spaces *> px <* spaces <* char ')'

atom :: Parser Expr
atom = choice "expecting expression"
    [ value
    , variable
    , parentheses expression ]

term :: Parser Expr
term = chainl1 (atom <* spaces) ops
    where
        ops = choice "expecting operator"
            [ Mul <$ (spaces *> char '*' <* spaces)
            , Div <$ (spaces *> char '/' <* spaces) ]

letExpr :: Parser Expr
letExpr = do
    _ <- string "let" <* spaces
    x <- identifier <* spaces
    _ <- char '=' <* spaces
    a <- expression <* spaces
    _ <- string "in" <* spaces
    b <- expression
    pure $ Let x a b

expression :: Parser Expr
expression = choice "expecting expression"
    [ letExpr
    , chainl1 term ops ]
    where
        ops = choice "expecting operator"
            [ Add <$ (spaces *> char '+' <* spaces)
            , Sub <$ (spaces *> char '-' <* spaces) ]

parse :: String -> Either String Expr
parse str =
    let px = spaces *> expression <* spaces <* void in
    let s0 = State str 0 in
    case runParser px s0 of
        Failure err -> Left $ diagnose s0 err
        Success x _ -> Right x
