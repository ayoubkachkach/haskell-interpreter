type Binding = [(Name, LExpr)]
type Evaluator = LExpr -> Binding -> LExpr
type Executor = LExpr -> Binding -> LExpr -> Binding

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


errorMsg :: Maybe LExpr -> String -> String
errorMsg (Just a) extraMsg = "Error evaluating node (" ++ show a ++ ")." ++ extraMsg
errorMsg Nothing extraMsg = "Error." ++ extraMsg

evalB :: Evaluator
evalB (B b) _ = (B b)
evalB expr _  = error (errorMsg (Just expr) " Expected bool literal.")

evalI :: Evaluator
evalI (I i) _ = (I i)
evalI expr _  = error (errorMsg (Just expr) " Expected int literal.")

evalEq :: Evaluator
evalEq x y b = (B (eval x b) == (eval y b))

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

eval :: Evaluator
eval (I i) _            = (I i)
eval (B b) _            = (B b)
eval (V v) b            = evalV (V v) b
eval expr b | isOp expr = eval (I i) _            = (I i)
evalOp expr b 