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

evalNot :: Evaluator
evalNot expr b =
    case eval expr b of
        (B b) -> (B (not b))
        _     -> error (errorMsg (Just expr) " Expected boolean.")
        
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
    (IF _ _ _)       -> evalIf expr b
    (LET _ _)        -> evalLet expr b
    expr | isOp expr -> evalOp expr b 