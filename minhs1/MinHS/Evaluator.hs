module MinHS.Evaluator where
  import qualified MinHS.Env as E
  import MinHS.Syntax
  import MinHS.Pretty
  import qualified Text.PrettyPrint.ANSI.Leijen as PP
  
  type VEnv = E.Env Value
  
  data Value = I Integer
             | B Bool
             | Nil
             | Cons Integer Value
             -- Others as needed
             deriving (Show)
  
  instance PP.Pretty Value where
    pretty (I i) = numeric $ i
    pretty (B b) = datacon $ show b
    pretty (Nil) = datacon "Nil"
    pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
    pretty _ = undefined -- should not ever be used
  
  evaluate :: Program -> Value
  evaluate [Bind _ _ _ e] = evalE E.empty e
  evaluate bs = evalE E.empty (Let bs (Var "main"))
  
  evalE :: VEnv -> Exp -> Value
  -- Constants and Boolean Constructors
  evalE env (Num value) = I value
  --evalE env (Con "True") = B True
  --evalE env (Con "False") = B False
  evalE env (Con bool) 
    | bool == "True" = B True
    | bool == "False" = B False
  
  -- Primitive Operations
  evalE env (App (App (Prim Op) e1) e2) = 
    let I val1 = evalE env e1 
        I val2 = evalE env e2
    in case Op of
      (Add)  -> I (val1 + val2)
      (Sub)  -> I (val1 - val2)
      (Mul)  -> I (val1 * val2)
      (Quot) -> case val2 of
                  (0)  -> error "Division by 0"
                  _   -> I (quot val1 val2) 
      (Rem)  -> case val2 of 
                  (0)  -> error "Division by 0"
                  _   -> I (rem val1 val2)
      (Gt)   -> B (val1 > val2)
      (Ge)   -> B (val1 >= val2)
      (Lt)   -> B (val1 < val2)
      (Le)   -> B (val1 <= val2)
      (Eq)   -> B (val1 == val2)
      (Ne)   -> B (val1 /= val2)
  evalE env (App (Prim Neg e1)) =
    let I val = evalE env e1
    in I (negate(val))
  
  -- Evaluation of if-expression
  evalE env (If cond exprT exprF) =
    let B bool = evalE env cond 
    in case bool of
      (B True)  -> evalE env exprT 
      (B False) -> evalE env exprF
  
  -- Variables
  evalE env (Var ident) = 
    let I val = evalE E.lookup ident 
    in case val of 
      Just v -> v
      _      -> error "Variable" ++ ident ++ "not in scope"
  
  -- List constructors and primops
  evalE env (Con "Nil") = Nil
  evalE env (App (App (Con "Cons") integer) value) = 
    let I val1 = evalE env integer 
    in Cons val1 (evalE env value)
  evalE env (App (Prim Head) list) = 
    case evalE env list of 
      (Cons v _) -> I v 
      (Nil)      -> error "Empty list"
  evalE env (App (Prim Tail) list) = 
    case evalE env list of 
      (Cons _ vs) -> vs 
      (Nil)       -> error "Empty list"
  evalE env (App (Prim Null) list) =
    case evalE env list of 
      (Cons _ _) -> B False
      (Nil)      -> B True
  
  -- Variable Bindings with Let 
  --evalE env (Let [] expr) = evalE env expr
  --evalE env (Let (Bind ident _ _ expr1) expr2) = evalE (E.add env (ident, (evalE env expr1)))(Let bs expr2)
  evalE env (Let list expr2) 
    | list == [] = evalE env expr2
    | list == ((Bind ident _ _ expr1):bs) = evalE (E.add env (ident, (evalE env expr1))) (Let bs expr2)
    -- |  need deal with recfun
  
  -- Function 
  -- Function Application
  