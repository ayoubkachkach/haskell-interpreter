module Scanner where
import Data.List
import System.IO
import Data.Char
import Debug.Trace


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
        
-----------------------------------------------------------------------------

--Returns (isSuccess, rest) where:
--isSuccess: Returns True iff the tokens given represent a valid arithmetic expression
--rest: rest of tokens after Arithmetic Expression has been (partially or fully) consumed.
--if isSuccess is true, rest should be empty
areValidExprTokens :: [String] -> (Bool, [String])
areValidExprTokens tokens =
    let
        exprConsumed = consumeExpr tokens
    in
        case tokens of
            []                                                             -> (False, [])
            tokens       | (fst exprConsumed) && (null (snd exprConsumed)) -> exprConsumed
            tokens                                                         -> (False, (snd exprConsumed))

--Returns (isSuccess, rest) where:
--isSuccess: is a boolean showing whether next Expression was fully consumed successfully
--rest: rest of tokens after Expression has been (partially or fully) consumed
consumeExpr :: [String] -> (Bool, [String])
consumeExpr tokens =
    let
        varConsumed = consumeVar tokens
        numConsumed = consumeNum tokens
        unConsumed = consumeUnExpr tokens
        binConsumed = consumeBinaryExpr tokens
    in
        case tokens of
            []                               -> (False, [])
            tokens       | (fst varConsumed) -> varConsumed
            tokens       | (fst numConsumed) -> numConsumed
            -- tokens       | (fst unConsumed)  -> unConsumed
            tokens       | (fst unConsumed)  -> unConsumed
            tokens       | (fst binConsumed) -> binConsumed
            ("(":"-":rest)                   -> unConsumed
            tokens                           -> binConsumed

--Returns (isSuccess, rest) where:
--isSuccess: is a boolean showing whether next Binary Expression was fully consumed successfully
--rest: rest of tokens after Binary Expression has been (partially or fully) consumed
consumeBinaryExpr :: [String] -> (Bool, [String])
consumeBinaryExpr tokens =
    case tokens of
        []              -> (False, [])
        ("(":"-":rest)  -> (False, ("-" : rest))
        ("(":"+":rest)  ->
                        let 
                                consumedExpr1 = consumeExpr rest
                                consumedExpr2 = consumeExpr (snd consumedExpr1)
                                isExpr1 = fst consumedExpr1
                                restConsumed1 = snd consumedExpr1
                                isExpr2 = fst consumedExpr2
                                restConsumed2 = snd consumedExpr2
                                toReturn = if (not isExpr1) then (False, restConsumed1)
                                           else if(not isExpr2) then (False, restConsumed2)
                                           else if ((sHead restConsumed2) == ")") then (True, (drop 1 restConsumed2))
                                           else (False, restConsumed2)
                        in
                          toReturn
        ("(":rest)     -> (False, rest)
        tokens         -> (False, tokens)

--Returns (isSuccess, rest) where:
--isSuccess: is a boolean showing whether next Unary Expression was fully consumed successfully
--rest: rest of tokens after Unary Expression has been (partially or fully) consumed
consumeUnExpr :: [String] -> (Bool, [String])
consumeUnExpr tokens = 
    case tokens of
        []              -> (False, [])
        ("(":"+":rest)  -> (False, ("+" : rest))
        ("(":"-":rest)  -> 
                        let 
                                consumedExpr = consumeExpr rest
                                isExpr = fst consumedExpr
                                restConsumed = snd consumedExpr
                                toReturn = if ((isExpr) && (sHead restConsumed) == ")") then (True, (drop 1 restConsumed))
                                           else (False, restConsumed)
                        in
                          toReturn
        ("(":rest)     -> (False, rest)
        tokens         -> (False, tokens)

--Returns (isSuccess, rest) where:
--isSuccess: is a boolean showing whether next Var was consumed successfully
--rest: rest of tokens after Var has been consumed
consumeVar :: [String] -> (Bool, [String])
consumeVar tokens =
    case tokens of
        []                           -> (False, [])
        (first:rest) | (isVar first) -> (True, rest)
        otherwise                    -> (False, otherwise)

--Returns (isSuccess, rest) where:
--isSuccess: is a boolean showing whether next Num was consumed successfully
--rest: rest of tokens after Num has been consumed
consumeNum :: [String] -> (Bool, [String])
consumeNum tokens =
    case tokens of
        []                           -> (False, [])
        (first:rest) | (isNum first) -> (True, rest)
        otherwise                    -> (False, otherwise)

--Returns True if given Variable is valid
isVar :: String -> Bool
isVar token = 
  case token of
    token        | not (isOfValidLength token 0 3)             -> False 
    token@(a:"") | isAlpha a                                   -> True
    token@(a:b)  | (isAlpha a) && (isAlphaOrNumber (head b))   -> True
    token                                                      -> False

--Returns True if given Variable is valid
isNum :: String -> Bool
isNum token = 
  case token of
    token        | not (isOfValidLength token 0 3)         -> False 
    token@(a:"") | isDigit a                               -> True
    token@(a:b)  | (isDigit a) && (isDigit (head b))       -> True
    token                                                  -> False

--Returns True iff given String's length is between lower and upper (exclusively)
isOfValidLength :: String -> Int -> Int -> Bool
isOfValidLength token lower upper = let
                                      len = length token
                                    in
                                      (len > lower) && (len < upper)

--Returns True iff given character is either a number of a letter
isAlphaOrNumber :: Char -> Bool
isAlphaOrNumber ch = (isAlpha ch) || (isNumber ch)

--Safe head: returns "" if list is empty. Behaves like head otherwise.
sHead :: [String] -> String
sHead l =
  case l of
    [] -> ""
    l -> (head l)

--For testing:
--Checks if two lists of tuples (Bool, [String]) are identical
alleq :: [(Bool, [String])] -> [(Bool, [String])] -> Bool
alleq [] [] = True
alleq _ [] = False
alleq [] _ = False
alleq (h1:t1) (h2:t2) = (fst h1 == fst h2) && (snd h1 == snd h2) && (alleq t1 t2)

--Test expressions
test = ["(+ a b)",
        "(- a)",
        "(- (+ 1 2))",
        "(+ (+ 1 2) (- a1))",
        "(a b)",
        "(+ (+ 1) (- a1))",
        "(+ (- 1 2) (- a1))",
        "(+ (- (+ (- a1) (+ a2 a3))) (- (+ a2 a3)))",
        "(+ (- (+ (- a1) (+ a2 a3))) (- (+ a2 a3))) a"
      ]

--Expected output from parser
expected = [(True, []),
            (True, []),
            (True, []),
            (True, []),
            (False, ["a", "b", ")"]),
            (False, [")", "(", "-", "a1", ")", ")"]),
            (False, ["2", ")", "(", "-", "a1", ")", ")"]),
            (True, []),
            (False, ["a"])
          ]
--test
main = 
    let
      tokens = map tokenize test
      result = map areValidExprTokens tokens
      testResult = alleq result expected
    in
      do
        putStrLn ("Final verdict: " ++ (show testResult))