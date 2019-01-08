module Parser where
import Data.List
import System.IO
import Data.Char
import Text.Read
import Debug.Trace
    
type Binding = [(Name, LExpr)]
type Evaluator = LExpr -> Binding -> LExpr
type Executor = LExpr -> Binding -> LExpr -> Binding
type Parser a = [String] -> (Maybe a, [String])

type Name = String
data LExpr =  V Name
            | B Bool
            | I Integer
            | Eq LExpr LExpr
            | Add LExpr LExpr
            | Mult LExpr LExpr
            | Sub LExpr LExpr
            | Div LExpr LExpr
            | And LExpr LExpr
            | Or LExpr LExpr
            | Not LExpr
            | L Name LExpr
            | FA LExpr LExpr
            | IF LExpr LExpr LExpr
            | LET (Name, LExpr) LExpr
            deriving (Eq, Show)

--input: list of tokens
--output: (Maybe firstToken, restTokens)
--i.e. parses 1st token from list of tokens
anyToken :: Parser String
anyToken (tkn:rest) = (Just tkn, rest)
anyToken []         = (Nothing, [])

--input: a token
--output: a parser that wraps the given token in a Just and puts it
--        in a tuple with the tokens it is given
--e.g. (result "a") ["b", "c"] ==> (Just "a", ["b", "c"])
result :: a -> Parser a
result x = \tkns -> (Just x, tkns)

--input: a parser P and a function FN
--       FN should take a token and return a parser
--output: a parser that applies P on the tkns it is given
--        -> if P was applied successfully: Apply FN on the token resulting the 
--           parse 
--        ->if P was not applied successfully: return the tokens unparsed
andThen :: Parser a -> (a -> Parser b) -> Parser b
andThen p fn =
  \tkns ->
    case p tkns of
      (Nothing, _)   -> (Nothing, tkns)
      (Just a, rest) -> (fn a) rest

--input: parser p of 'a' and function fn 'a->b'
--output: a parser of 'b' which takes a list of tokens and applies fn on them
mapTo :: Parser a -> (a -> b) -> Parser b
mapTo p fn = andThen p (\a -> result (fn a))


--input: parser 'p' and predicate 'pred'
--output: returns parsed token with 'p' if 'pred' is true on parsed token. Else or if p fails, it returns (Nothing, tkns)
filterBy :: Parser a -> (a -> Bool) -> Parser a
filterBy p pred =
  \tkns ->
    case p tkns of
      (Nothing, rest) -> (Nothing, tkns)
      (Just x, rest) -> if pred x then (Just x, rest)
                        else (Nothing, tkns)
                        
tokenizeAny :: [String] -> Parser String
tokenizeAny tokens =
  case tokens of
    [] -> token ""
    (fst:rest) -> foldr orElse (token fst) (map token rest)

--Very powerful!
--input: parser 'a'
--output: returns parser of ['a']
--summary: parses tokens of type'a' until it fails
multiple :: Parser a -> Parser [a]
multiple p =
  \tkns ->
    let
      (mbP, pRest) = p tkns
    in
      case mbP of
        Nothing -> (Just [], pRest)
        Just pR -> 
          case multiple p pRest of
            (Just results, rest) -> (Just (pR:results), rest)
            (Nothing, rest)      -> (Nothing, rest)

--input: token to parse for and list of tokens
--output: parsed tokens
token :: String -> Parser String
token tkn = anyToken `filterBy` (== tkn)

--combines two given parsers using or
--e.g.: If given an "A" and "B" parsers, it will give a parser
--that parses either "A" or "B"
orElse :: Parser a -> Parser a -> Parser a
orElse p q = 
  \tkns ->
    let
      (pF, pRest) = p tkns
      (qF, qRest) = q tkns
    in
      case pF of
        Nothing -> (qF, qRest)
        Just x  -> (pF, pRest)

-- ***********************************************************

twoTokens :: Parser String
twoTokens = andThen anyToken (\w1 -> andThen anyToken (\w2 -> result (w1 ++ w2)))

anyTokenLength :: Parser Int
anyTokenLength = anyToken `mapTo` length

open :: Parser String
open = anyToken `filterBy` (== "(")

close :: Parser String
close = anyToken `filterBy` (== ")")

isName :: String -> Bool
isName (s:rest) = ((s >= 'a') && (s <= 'z'))
isName []       = False

name :: Parser String
name = anyToken `filterBy` isName

nameVar :: Parser LExpr
nameVar = name `mapTo` (\tkn -> V tkn)

isInt :: String -> Bool
isInt tkn = 
  case (readMaybe tkn :: Maybe Integer) of
      Nothing -> False
      _       -> True
              
toInt :: String -> Integer
toInt tkn =
  case (readMaybe tkn :: Maybe Integer) of
      Just x  -> x

intValue :: Parser LExpr
intValue = anyToken `filterBy` isInt `mapTo` (\tkn -> I (toInt tkn))

toBool :: String -> Bool
toBool "True" = True
toBool "False" = False

isBool :: String -> Bool
isBool tkn = any (tkn==) ["True", "False"]

boolVal :: Parser LExpr
boolVal = anyToken `filterBy` isBool `mapTo` (\tkn -> B (toBool tkn))

op :: Parser String
op = parseAnyToken ["Add", "Mult", "Div", "Sub", "And", "Or"]

getBinOpConstructor :: String -> Maybe(LExpr -> LExpr -> LExpr)
getBinOpConstructor opStr = lookup opStr opStrToConstrustor
                          where opStrToConstrustor = [("Add", Add), ("Mult", Mult), ("Div", Div), ("Sub", Sub), ("And", And), ("Or", Or)]

binOperation :: Parser LExpr
binOperation = open `andThen` \_ ->
                op `andThen` \operator ->
                  lExpr `andThen` \expr1 ->
                    lExpr `andThen` \expr2 -> 
                      close `andThen` \_ ->
                        let Just opConstructor = getBinOpConstructor operator
                        in 
                            result (opConstructor expr1 expr2)

notOperation :: Parser LExpr
notOperation = open `andThen` \_ ->
                (token "Not") `andThen` \_ ->
                  lExpr `andThen` \expr ->
                    close `andThen` \_ ->
                      result (Not expr)

eqExpr :: Parser LExpr
eqExpr = open `andThen` \_ ->
          (token "Eq") `andThen` \_ ->
            lExpr `andThen` \expr1 ->
              lExpr `andThen` \expr2 ->
                close `andThen` \_ ->
                  result (Eq expr1 expr2)
                  
lambdaExpr :: Parser LExpr
lambdaExpr = open `andThen` \_ ->
              (token "L") `andThen` \_ ->
                name `andThen` \var ->
                  (token ".") `andThen` \_ ->
                    lExpr `andThen` \expr ->
                      close `andThen` \_ ->
                        result (L var expr)

funcExpr :: Parser LExpr
funcExpr = open `andThen` \_ ->
            lExpr `andThen` \expr1 ->
              lExpr `andThen` \expr2 ->
                close `andThen` \_ ->
                  result (FA expr1 expr2)
                
ifExpr :: Parser LExpr
ifExpr = open `andThen` \_ ->
          (token "IF") `andThen` \_ ->
            lExpr `andThen` \expr1 ->
              lExpr `andThen` \expr2 ->
                lExpr `andThen` \expr3 ->
                  close `andThen` \_ ->
                    result (IF expr1 expr2 expr3)

letExpr :: Parser LExpr
letExpr = open `andThen` \_ ->
            (token "LET") `andThen` \_ ->
              name `andThen` \var ->
                (token "=") `andThen` \_ ->
                  lExpr `andThen` \expr1 ->
                    (token "IN") `andThen` \_->
                      lExpr `andThen` \expr2 ->
                        close `andThen` \_ ->
                          result (LET (var, expr1) expr2)
                    
-- given a list of parsers, a new parser is created s.t. it can parse a given token if at least one of the parsers can parse it
parseAny :: [Parser a] -> Parser a
parseAny parsers =
  case parsers of
    [] -> (\tkns -> (Nothing, tkns)) 
    (fst:rest) -> foldr orElse fst rest

-- creates a parser that parses any of the string in the given list
parseAnyToken :: [String] -> Parser String
parseAnyToken tokens = parseAny (map token tokens)

lExpr :: Parser LExpr
lExpr = parseAny [nameVar, boolVal, binOperation, notOperation, eqExpr, lambdaExpr, funcExpr, ifExpr, letExpr, intValue]
-- lExpr = nameVar `orElse` boolVal `orElse` binOperation `orElse` notOperation `orElse` eqExpr `orElse` lambdaExpr `orElse` funcExpr `orElse` ifExpr `orElse` letExpr
                
parse :: Parser LExpr
parse = lExpr

-- tokenize --
tokenize :: String -> [String]
tokenize "" = []
tokenize text = tokenize' (nextWordAndRest text)
      where
          tokenize' :: (String, String) -> [String]
          tokenize' (first,"") = [first]
          tokenize' (first,rest) = [first] ++ (tokenize rest)



-- skipWhiteSpaces -------------------------------------------------------------

skipWhiteSpaces :: String -> String
skipWhiteSpaces (c:cs) | isSpace c   = skipWhiteSpaces cs
skipWhiteSpaces cs                   = cs

------------------------------------------------------------------------------

nextWordAndRest :: String -> (String, String)
nextWordAndRest text = nextWordAndRest' (skipWhiteSpaces text) ""
    where
        nextWordAndRest' :: String -> String -> (String, String)
        nextWordAndRest' []           word              = (reverse word, [])
        nextWordAndRest' txt@('(':rest)  []              = ("(",rest)
        nextWordAndRest' txt@(')':rest)  []              = (")",rest)
        nextWordAndRest' txt@('(':_)  word              = (reverse word, txt)
        nextWordAndRest' txt@(')':_)  word              = (reverse word, txt)
        nextWordAndRest' (s:rest)     word | isSpace s  = (reverse word, rest)
        nextWordAndRest' (c:rest)     word              = nextWordAndRest' rest (c:word)

main :: IO ()
main = return ()