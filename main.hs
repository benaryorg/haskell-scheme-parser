import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readOct,readHex)
import System.Environment
import Control.Monad.Except

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany space

escape :: Parser Char
escape = do
	char '\\'
	ch <- oneOf "\\\"nrt"
	return $ case ch of
		'\\' -> '\\'
		'n' -> '\n'
		'r' -> '\r'
		't' -> '\t'

stringChar :: Parser Char
stringChar = escape <|> noneOf "\""

charName :: String -> Char
charName "" = ' '
charName "space" = ' '
charName "newline" = '\n'
charName (ch:[]) = ch

readBin :: String -> Integer
readBin "" = 0
readBin "0" = 0
readBin "1" = 1
readBin s = 2 * (readBin $ init s) + readBin [last s]

data LispVal = Atom String
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer
	| String String
	| Character Char
	| Bool Bool
	| Undef

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
	| TypeMismatch String LispVal
	| Parser ParseError
	| BadSpecialForm LispVal
	| NotAFunction String
	| UnboundVar String
	| Other String

instance Show LispError where
	show (NumArgs expected found) = "wrong number of arguments: expected " ++ show expected
		++ ", found (" ++ unwordsList found ++ ")"
	show (TypeMismatch expected found) = "type mismatch: expected type " ++ expected ++ ", found " ++
		show found
	show (Parser err) = "parser error: " ++ show err
	show (BadSpecialForm form) = "unrecognized special form: " ++ show form
	show (NotAFunction func) = "not a function: " ++ func
	show (UnboundVar varname) = "unbound variable: " ++ varname

type LispMonad = Either LispError

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many stringChar
	char '"'
	return $ String x

parseCharacter :: Parser LispVal
parseCharacter = do
	string "#\\"
	name <- many letter
	notFollowedBy alphaNum
	return $ (Character . charName) name

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> symbol <|> digit)
	let atom = first:rest
	return $ case atom of
		"#t" -> Bool True
		"#f" -> Bool False
		_ -> Atom atom

parseBin :: Parser LispVal
parseBin = fmap (Number . readBin) . many1 $ oneOf "01"

parseOct :: Parser LispVal
parseOct = do
	n <- many1 octDigit
	notFollowedBy alphaNum
	return $ (Number . fst . head . readOct) n

parseDec :: Parser LispVal
parseDec = fmap (Number . read) $ many1 digit

parseHex :: Parser LispVal
parseHex = do
	n <- many1 hexDigit
	notFollowedBy alphaNum
	return $ (Number . fst . head . readHex) n

parsePrefixNumber :: Parser LispVal
parsePrefixNumber = do
	char '#'
	ch <- oneOf "bodx"
	case ch of
		'b' -> parseBin
		'o' -> parseOct
		'd' -> parseDec
		'x' -> parseHex

parseNumber :: Parser LispVal
parseNumber = parsePrefixNumber
	<|> do
		char '-'
		Number n <- parseDec
		return . Number $ -1 * n
	<|> parseDec

parseList :: Parser LispVal
parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
	head <- endBy parseExpr spaces
	tail <- char '.' >> spaces >> parseExpr
	return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
	char '\''
	x <- parseExpr
	return $ List [Atom "quote",x]

parseExpr :: Parser LispVal
parseExpr = try parseNumber
	<|> try parseCharacter
	<|> try parseString
	<|> try parseQuoted
	<|> try parseAtom
	<|> between (char '(') (char ')') (try parseList <|> try parseDottedList)

readExpr :: String -> LispMonad LispVal
readExpr input = either (throwError . Parser) (return)
	$ parse (spaces >> parseExpr) "lisp" input

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

showVal :: LispVal -> String
showVal (String val) = "\"" ++ val ++ "\""
showVal (Atom name) = name
showVal (Number val) = show val
showVal (Character val) = show val
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List list) = "(" ++ unwordsList list ++ ")"
showVal (DottedList list tail) = "(" ++ unwordsList list ++ " . " ++ show tail ++ ")"
showVal (Undef) = "#<undef>"

eval :: LispVal -> LispMonad LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Character _) = return val
eval val@(Bool _) = return val
eval val@(Undef) = return val
eval (List [Atom "quote",val]) = return val
eval (List [Atom "if",condition,true]) = eval $ List [Atom "if",condition,true,Undef]
eval (List [Atom "if",condition,true,false]) = do
	condition <- eval condition
	case condition of
		Bool False -> eval true
		_ -> eval false
eval (List (Atom "if":xs)) = throwError $ NumArgs 3 xs
eval (List [Atom "car",List (x:xs)]) = return x
eval (List [Atom "car",DottedList (x:xs) _]) = return x
eval (List (Atom func:args)) = mapM eval args >>= apply func
eval other = throwError $ BadSpecialForm other
--eval DottedList
--eval Atom

primitives :: [(String,[LispVal] -> LispMonad LispVal)]
primitives =
	[
		("+",numericBinOp (+)),
		("-",numericBinOp (-)),
		("*",numericBinOp (*)),
		("/",numericBinOp (div)),
		("mod",numericBinOp (mod)),
		("quotient",numericBinOp (quot)),
		("remainder",numericBinOp (rem)),
		("=",numericBoolOp (==)),
		("<",numericBoolOp (<)),
		(">",numericBoolOp (>)),
		("/=",numericBoolOp (/=)),
		(">=",numericBoolOp (>=)),
		("<=",numericBoolOp (<=)),
		("&&",boolBoolOp (&&)),
		("||",boolBoolOp (||)),
		("string=?",stringBoolOp (==)),
		("string<?",stringBoolOp (<)),
		("string>?",stringBoolOp (>)),
		("string<=?",stringBoolOp (<=)),
		("string>=?",stringBoolOp (>=))
	]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispMonad LispVal
numericBinOp op val@[] = throwError $ NumArgs 2 val
numericBinOp op val@[_] = throwError $ NumArgs 2 val
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

boolOp :: (LispVal -> LispMonad a) -> (a -> a -> Bool) -> [LispVal] -> LispMonad LispVal
boolOp unpacker op [x,y] = do
	x <- unpacker x
	y <- unpacker y
	return $ Bool (x `op` y)
boolOp unpacker op val = throwError $ NumArgs 2 val

numericBoolOp = boolOp unpackNum
boolBoolOp = boolOp unpackBool
stringBoolOp = boolOp unpackStr

unpackNum :: LispVal -> LispMonad Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "Number" notNum

unpackBool :: LispVal -> LispMonad Bool
unpackBool (Bool n) = return n
unpackBool notBool = throwError $ TypeMismatch "Bool" notBool

unpackStr :: LispVal -> LispMonad String
unpackStr (String n) = return n
unpackStr notStr = throwError $ TypeMismatch "String" notStr

apply :: String -> [LispVal] -> LispMonad LispVal
apply func args = maybe (throwError $ NotAFunction func) ($ args) $ lookup func primitives

exec :: String -> String
exec s = either (show) (show) $ join . (fmap eval) $ readExpr s

--{-
main :: IO ()
main = getArgs >>= putStrLn . exec . head
---}
{-
main :: IO String
main = getArgs >>= return . exec . head
---}

