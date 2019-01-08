module Interpreter where
import Parser
import System.IO

-- type Binding = [(Name, LExpr)]
-- type Evaluator = LExpr -> Binding -> LExpr
-- type Executor = LExpr -> Binding -> LExpr -> Binding
-- type Parser a = [String] -> (Maybe a, [String])

-- type Name = String
-- data LExpr =  V Name
--             | B Bool
--             | I Integer
--             | Eq LExpr LExpr
--             | Add LExpr LExpr
--             | Mult LExpr LExpr
--             | Sub LExpr LExpr
--             | Div LExpr LExpr
--             | And LExpr LExpr
--             | Or LExpr LExpr
--             | Not LExpr
--             | L Name LExpr
--             | FA LExpr LExpr
--             | IF LExpr LExpr LExpr
--             | LET (Name, LExpr) LExpr
--             deriving (Eq, Show)


errorMsg :: Maybe LExpr -> String -> String
errorMsg (Just a) extraMsg = "Error evaluating node (" ++ show a ++ ")." ++ extraMsg
errorMsg Nothing extraMsg = "Error." ++ extraMsg

evalB :: Evaluator
evalB (B b) _ = (B b)
evalB expr _  = error (errorMsg (Just expr) " Expected bool literal.")

evalI :: Evaluator
evalI (I i) _ = (I i)
evalI expr _  = error (errorMsg (Just expr) " Expected int literal.")

evalV :: Evaluator
evalV (V v) bindings = 
    case lookup v bindings of
      Just val -> eval val bindings  -- found variable, lazy eval
      Nothing  -> error (errorMsg (Just (V v)) " Unbound variable.") -- undefined variable

extractOp :: LExpr -> Maybe (String, LExpr, LExpr)
extractOp expr =
  case expr of
    (Add a b)  -> Just ("Add", a, b)
    (Mult a b) -> Just ("Mult", a, b)
    (Sub a b)  -> Just ("Sub", a, b)
    (Div a b)  -> Just ("Div", a, b)
    (And a b)  -> Just ("And", a, b)
    (Or a b)   -> Just ("Or", a, b)
    otherwise  -> Nothing

isOp :: LExpr -> Bool 
isOp expr =
  case extractOp expr of
    Nothing -> False
    Just _  -> True

iOp :: (Integer -> Integer -> Integer) -> LExpr -> LExpr -> LExpr
iOp op (I a) (I b) = (I (op a b))
iOp _ _ _          = error (errorMsg Nothing " Couldn't resolve binary operation. Expected integers.")

bOp :: (Bool -> Bool -> Bool) -> LExpr -> LExpr -> LExpr
bOp op (B a) (B b) = (B (op a b))
bOp _ _ _          = error (errorMsg Nothing " Couldn't resolve binary operation. Expected booleans.")

evalEq :: Evaluator
evalEq (Eq x y) bind = let a = eval x bind
                           b = eval y bind
                       in
                        case (a, b) of
                          ((B b1), (B b2)) -> (B (b1 == b2))
                          ((I i1), (I i2)) -> (B (i1 == i2))
                          (_, _)           -> error (errorMsg Nothing " Comparing incompatible types.")

evalNot :: Evaluator
evalNot expr b =
    case eval expr b of
        (B b) -> (B (not b))
        _     -> error (errorMsg (Just expr) " Expected boolean.")
        
evalOp :: Evaluator
evalOp expr bindings =
  let 
    opStrToOp  = [("Add", (iOp (+))), ("Sub", (iOp (-))), ("Mult", (iOp (*))), ("Div", (iOp (div))), ("And", (bOp (&&))), ("Or", (bOp (||)))]
  in
    case extractOp expr of
      Nothing             -> error (errorMsg (Just expr) " Expected binary operation.")
      Just (opStr, a, b)  -> let Just op = lookup opStr opStrToOp 
                                 x = eval a bindings
                                 y = eval b bindings
                             in 
                                 op x y
evalIf :: Evaluator
evalIf (IF cond expT expF) b =
  case eval cond b of
    (B True)  -> eval expT b
    (B False) -> eval expF b
    _         -> error (errorMsg (Just cond) " Can't reduce condition to boolean.")

addOrReplace :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
addOrReplace key value assoc = (key,value):(filter ((key /=).fst) assoc)

evalLet :: Evaluator
evalLet expr bindings = 
    case expr of
        (LET (var, val) expr) -> eval expr (addOrReplace var val bindings)
        _                     -> error (errorMsg (Just expr) " Expected valid LET expression.")

    
eval :: Evaluator
eval expr b =
  case expr of
    (I _)            -> expr
    (B _)            -> expr
    (V _)            -> evalV expr b
    (Eq _ _)         -> evalEq expr b
    (FA _ _)         -> evalFA expr b
    (L _ _)          -> expr
    (IF _ _ _)       -> evalIf expr b
    (LET _ _)        -> evalLet expr b
    expr | isOp expr -> evalOp expr b 

betaReduce :: Evaluator
betaReduce expr b = 
  case expr of
    (I _)            -> expr
    (B _)            -> expr
    (Not n)          -> (Not (betaReduce n b))
    (Eq x y)         -> (Eq (betaReduce x b) (betaReduce y b))
    (L x y)          -> (L x (betaReduce y b))
    (IF x y z)       -> (IF (betaReduce x b) (betaReduce y b) (betaReduce z b))
    (LET (v, x) y)   -> (LET (v, (betaReduce x b)) (betaReduce y b))
    (FA x y)         -> (FA (betaReduce x b) (betaReduce y b))
    expr | isOp expr -> let 
                          Just (opStr, x, y) = extractOp expr
                          opStrToOp  = [("Add", Add), ("Sub", Sub), ("Mult", Mult), ("Div", Div), ("And", And), ("Or", Or)]
                          Just opConst = lookup opStr opStrToOp
                        in 
                          (opConst (betaReduce x b) (betaReduce y b))
    (V v)            -> case lookup v b of 
                          Nothing -> expr
                          Just x  -> (betaReduce x b)

evalFA :: Evaluator
evalFA (FA f arg) b =
  case eval f b of
    (L var lamb) -> let 
                      new_b = addOrReplace var arg b
                      beta_reduced_lamb = betaReduce lamb new_b
                    in
                      eval beta_reduced_lamb new_b
    _  -> error (errorMsg (Just f) " Couldn't reduce expr to lambda expression.")
    
evalFA expr  _ = error (errorMsg (Just expr) " Expected function application.")

main :: IO ()
main = let 
          tokenizedCode = tokenize "(LET square = (L x . (Mult x x)) IN (LET sumSquares = (L a . (L b . (Add (square a) (square b)))) IN ((sumSquares 3) 4)))"
          Just parsedCode = fst (parse tokenizedCode)
          result = eval parsedCode []
        in
          putStrLn (show result) 