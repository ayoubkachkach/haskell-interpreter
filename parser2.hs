type Parser a = [String] -> (Maybe a, [String])

data CST =
          Atom String
        | Op String [CST]
      deriving (Eq, Read, Show)

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
      
--Example of a parser of String tokens:
--input: list of tokens
--output: (Maybe String, [String])
--Example: twoTokens ["A", "B", "C"]
-- > (andThen anyToken \lambd1) tokens --> parsed successfully so: (\lambd1 "A") ["B", "C"]
-- > replacing \lambd1 with its definition:
-- > (andThen anyToken \lambd2) ["B", "C"] s.t w1 = "A"
-- > anyToken is applied first --> successful: so \lambd2 is called on the parsed token ("B")
-- > (\lambd2 "B") ["C"] --> (result (w1 ++ w2)) ["C"] s.t w1:"A", w2:"B"
-- > result "AB" ["C"]
twoTokens :: Parser String
twoTokens = andThen anyToken (\w1 -> andThen anyToken (\w2 -> result (w1 ++ w2)))

--input: parser p of 'a' and function fn 'a->b'
--output: a parser of 'b' which takes a list of tokens and applies fn on them
mapTo :: Parser a -> (a -> b) -> Parser b
mapTo p fn = andThen p (\a -> result (fn a))

--input: list of tokens
--output: (Maybe len, rest) s.t. len is the length of the parsed token and rest is the remaining tokens
anyTokenLength :: Parser Int
anyTokenLength = anyToken `mapTo` length
--input: parser 'p' and predicate 'pred'
--output: returns parsed token with 'p' if 'pred' is true on parsed token. Else or if p fails, it returns (Nothing, tkns)

filterBy :: Parser a -> (a -> Bool) -> Parser a
filterBy p pred =
  \tkns ->
    case p tkns of
      (Nothing, rest) -> (Nothing, tkns)
      (Just x, rest) -> if pred x then (Just x, rest)
                        else (Nothing, tkns)

--input: token to parse for and list of tokens
--output: parsed tokens
token :: String -> Parser String
token tkn = anyToken `filterBy` (== tkn)

open :: Parser String
open = anyToken `filterBy` (== "(")

close :: Parser String
close = anyToken `filterBy` (== ")")

word :: Parser String
word = anyToken `filterBy` (\tkn -> all (tkn /=) [")", "("])

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

atom :: Parser CST
atom = word `mapTo` (\w -> Atom w)

tokenizeAny :: [String] -> Parser String
tokenizeAny tokens =
  case tokens of
    [] -> token ""
    (fst:rest) -> foldr orElse (token fst) (map token rest)

tokenizeSigns :: Parser String
tokenizeSigns = tokenizeAny ["+", "-", "*", "/"]

operation :: Parser CST
operation = open `andThen` \oPar ->  
              tokenizeSigns `andThen` \sign -> 
                (multiple expr) `andThen` \exprs -> 
                  close `andThen` \cPar -> 
                    result (Op sign exprs)

expr :: Parser CST
expr = atom `orElse` operation

main :: IO ()
main = return ()
