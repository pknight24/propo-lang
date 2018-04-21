module Types.Expressions  where

data Expr
  = Atom Bool
  | And Expr Expr
  | Or  Expr Expr
  | Implies Expr Expr
  | Iff Expr Expr
  | Not Expr
  | Null
  deriving (Show, Read)

eval_base :: Expr -> Maybe Bool
eval_base (Atom a)      = Just a
eval_base (And a b)     = do
                            f <- eval_base a
                            s <- eval_base b
                            return (f && s)
eval_base (Or a b)      = do
                            f <- eval_base a
                            s <- eval_base b
                            return (f || s)
eval_base (Not a)       = do
                            f <- eval_base a
                            return $ not f
eval_base (Implies a b) = eval_base (Or (Not a) b)
eval_base (Iff a b)     = eval_base (And (Implies a b) (Implies b a))
eval_base Null          = Nothing
