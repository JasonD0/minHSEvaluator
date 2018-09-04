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
             | Closure VEnv Exp
             | PartialPrimOp Op Value 
             | PartialCons Value
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
  evalE env (Con bool)  | bool == "True" = B True
                        | bool == "False" = B False
  
  -- Primitive Operations
  -- errors  16     15, 13 hex -> prob diff for test machine
  evalE env (App (App (Prim operation) e1) e2) = 
    let I val1 = evalE env e1 
        I val2 = evalE env e2
    in case operation of
      (Add)  -> I (val1 + val2)
      (Sub)  -> I (val1 - val2)
      (Mul)  -> I (val1 * val2)
      (Quot) -> case val2 of
                  (0)  -> error "Division by 0"
                  _    -> I (quot val1 val2) 
      (Rem)  -> case val2 of 
                  (0)  -> error "Division by 0"
                  _    -> I (rem val1 val2)
      (Gt)   -> B (val1 > val2)
      (Ge)   -> B (val1 >= val2)
      (Lt)   -> B (val1 < val2)
      (Le)   -> B (val1 <= val2)
      (Eq)   -> B (val1 == val2)
      (Ne)   -> B (val1 /= val2)
  evalE env (App (Prim Neg) e1) =
    let I val = evalE env e1
    in I (negate(val))
  
  -- Evaluation of if-expression
  evalE env (If cond exprT exprF) =
    let B bool = evalE env cond 
    in case bool of
      (True)  -> evalE env exprT 
      (False) -> evalE env exprF
  
  -- Variables
  evalE env (Var ident) = 
    case E.lookup env ident of 
      (Just v) -> v
      _        -> error "Variable not in scope"
  
  -- List constructors and primops
  evalE env (Con "Nil") = Nil
  evalE env (App (App (Con "Cons") integer) value) = 
    let I val1 = evalE env integer 
    in Cons val1 (evalE env value)
  evalE env (App (Prim Head) list) = 
    let x = evalE env list 
    in case x of 
        (Cons v _) -> I v 
        (Nil)      -> error "Empty list"
  evalE env (App (Prim Tail) list) = 
    let x = evalE env list 
    in case x of 
        (Cons _ vs) -> vs 
        (Nil)       -> error "Empty list"
  evalE env (App (Prim Null) list) =
    let x = evalE env list 
    in case x of 
        (Cons _ _) -> B False
        (Nil)      -> B True
    
  -- Variable Bindings with Let 
  evalE env (Let [] expr) = evalE env expr  -- finished reading all bindings  env will have the binded values 
  evalE env (Let ((Bind ident types list expr1):bs) expr2) = evalE (E.add env (ident, (evalE env expr1))) (Let bs expr2)

  -- Function 
  evalE env (Recfun (Bind ident (Arrow typ1 typ2) list expr)) = Closure env (Recfun (Bind ident (Arrow typ1 typ2) list expr))
  evalE env (Recfun (Bind ident types [] expr)) = evalE (E.add env (ident, (evalE env expr))) expr 

  -- Function Application
  evalE env (App expr1 expr2) =
    let x = evalE env expr1
        I val2 = evalE env expr2
    in case x of 
        -- no more args, add function value to env then apply value of expr2 to function 
        (Closure exprEnv (Recfun (Bind ident (Arrow typ1 typ2) [] expr))) -> evalE (E.add exprEnv (ident, (Closure exprEnv (Recfun(Bind ident (Arrow typ1 typ2) [] expr))))) (App expr expr2)
        -- function still has args, add function value to env  and  add value of first arg (eval of expr2)
        (Closure exprEnv (Recfun (Bind ident types (a:as) expr)))         -> evalE (E.addAll exprEnv [(ident, (Closure exprEnv (Recfun (Bind ident types (a:as) expr)))), (a, (evalE env expr2))]) expr
        -- Partial Primops
        (PartialPrimOp Add (I val1))  -> I (val1 + val2)
        (PartialPrimOp Sub (I val1))  -> I (val1 - val2) 
        (PartialPrimOp Mul (I val1))  -> I (val1 * val2) 
        (PartialPrimOp Quot (I val1)) -> case val2 of
                                          (0) -> error "Division by zero"
                                          _   -> I (quot val1 val2)
        (PartialPrimOp Rem (I val1))  -> case val2 of 
                                          (0) -> error "Division by zero"
                                          _   -> I (rem val1 val2)
        (PartialPrimOp Neg (I val))   -> I (negate val) 
        (PartialPrimOp Gt (I val1))   -> B (val1 > val2)
        (PartialPrimOp Ge (I val1))   -> B (val1 >= val2)
        (PartialPrimOp Lt (I val1))   -> B (val1 < val2)
        (PartialPrimOp Le (I val1))   -> B (val1 <= val2) 
        (PartialPrimOp Eq (I val1))   -> B (val1 == val2)
        (PartialPrimOp Ne (I val1))   -> B (val1 /= val2)
        (PartialCons (I val1))        -> Cons val1 (evalE env expr2)  
        _                                                                 -> x

  -- Partial Primops
  --evalE env (App expr1 expr2) =
    --let x = evalE env expr1 
      --  I val2 = evalE env expr2
    --in case x of 
     -- (PartialPrimOp Add (I val1)) -> I (val1 + val2)
      --(PartialPrimOp Sub (I val1)) -> I (val1 - val2) 
      --(PartialPrimOp Mul (I val1)) -> I (val1 * val2) 
      --(PartialPrimOp Quot (I val1)) -> case val2 of
        --                                (0) -> error "Division by zero"
          --                              _   -> I (quot val1 val2)
      --(PartialPrimOp Rem (I val1)) -> case val2 of 
        --                                (0) -> error "Division by zero"
          --                              _   -> I (rem val1 val2)
      --(PartialPrimOp Neg (I val)) -> I (negate val) 
      --(PartialPrimOp Gt (I val1)) -> B (val1 > val2)
      ---(PartialPrimOp Ge (I val1)) -> B (val1 >= val2)
      --(PartialPrimOp Lt (I val1)) -> B (val1 < val2)
      --(PartialPrimOp Le (I val1)) -> B (val1 <= val2) 
      --(PartialPrimOp Eq (I val1)) -> B (val1 == val2)
      --(PartialPrimOp Ne (I val1)) -> B (val1 /= val2)
      --(PartialCons (I val1)) -> Cons val1 (evalE env expr2)