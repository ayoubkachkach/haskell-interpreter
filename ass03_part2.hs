type Binding = [(Name, LExpr)]
type Evaluator = LExpr -> Binding -> Bool -> LExpr

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
errorMsg Nothing extraMsg = "Error. ++ extraMsg

evalB ::
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
    Nothing = False
    Just _  = True

reduce 