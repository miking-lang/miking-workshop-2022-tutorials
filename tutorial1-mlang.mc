-- Includes from the standard library
include "common.mc"
include "mexpr/info.mc"
include "string.mc"


-- Includes from the local directory
include "./lib/lambda.mc"
include "./tutorial1-mexpr.mc"
include "./ast.mc"


/-

  The purpose of this tutorial is for you to familiarize yourself with MLang and
  language fragments in particular.

  This tutorial is divided into two tasks, with somewhat rising complexity. We
  will re-use your solution to Task A later in the tutorial.

-/



-- Helpers to convert between floats and Exprs

let exprToFloat = use NumExprAst in
  lam e. match e with NumExpr x in x.val.v
let floatToExpr = use NumExprAst in
  lam f. NumExpr {info = NoInfo (), val = {v = f, i = NoInfo()}}


-- Language fragments implementing 'eval'

lang EvalBase = CalcBaseAst
  sem eval : Map String Expr -> Expr -> Expr
end

lang NumExprEval = EvalBase + NumExprAst
  sem eval env =
  | e & NumExpr _ -> e
end

lang TermEval = EvalBase + AddExprAst + SubExprAst
  sem eval env =
  | AddExpr x ->
    let l = exprToFloat (eval env x.left) in
    let r = exprToFloat (eval env x.right) in
    floatToExpr (addf l r)
  | SubExpr x ->
    let l = exprToFloat (eval env x.left) in
    let r = exprToFloat (eval env x.right) in
    floatToExpr (subf l r)
end

lang FactorEval = EvalBase + MulExprAst + DivExprAst
  sem eval env =
  | MulExpr x ->
    let l = exprToFloat (eval env x.left) in
    let r = exprToFloat (eval env x.right) in
    floatToExpr (mulf l r)
  | DivExpr x ->
    let l = exprToFloat (eval env x.left) in
    let r = exprToFloat (eval env x.right) in
    floatToExpr (divf l r)
end


-- Language fragments implementing 'toString'

lang ToStringBase
  sem toString : Expr -> String
end

lang NumToString = ToStringBase + NumExprAst
  sem toString =
  | NumExpr x -> float2string x.val.v
end

lang TermToString = ToStringBase + AddExprAst + SubExprAst
  sem toString =
  | AddExpr x -> join ["(", toString x.left, " + ", toString x.right, ")"]
  | SubExpr x -> join ["(", toString x.left, " - ", toString x.right, ")"]
end

lang FactorToString = ToStringBase + MulExprAst + DivExprAst
  sem toString =
  | MulExpr x -> join ["(", toString x.left, " * ", toString x.right, ")"]
  | DivExpr x -> join ["(", toString x.left, " / ", toString x.right, ")"]
end

-- Composed languages

lang Eval = TermEval + NumExprEval + FactorEval
end

lang ToString = NumToString + TermToString + FactorToString
end

lang Complete = Eval + ToString
end




/-
  Task A ---------------------------------------------------------------------

  Extend the language fragments `CalcAst` (in ast.mc), `Eval`, and
  `ToString` with and AST node for sin(x). The sine function in mexpr
  is called `sin`. You should name the new language fragment in ast.mc
  `SinExprAst`, and the new constructor, `SinExpr` for the sake of the
  rest of the tutorials.

-/

/-
  Task B ---------------------------------------------------------------------

  Extend the language fragments `CalcAst` (in ast.mc), `Eval`, and
  `ToString` with and AST node for integer exponents based on your
  implementation of `pow` from `tutorial1-mexpr.mc`. You should name the
  new language fragment in ast.mc `PowExprAst`, and the new constructor
  `PowExpr`, for the sake of the rest of the tutorials.

  You will probably need to convert from floating point numbers to
  integers which you can do with the functions `floorfi`, `ceilfi`, or
  `roundfi`.

-/

/-
  Task C (tutorial session 2) ------------------------------------------------

  Add concrete syntax for your expressions using `ast.syn`. Replace
  `include "ast.mc"` with `include "ast-gen.mc"` to use the generated
  AST language fragments instead of the manually written ones, along
  with a parsing function.

  Next, use the (generated) function `parseCalcExn` to write your
  expressions, instead of the helpers below. For example:

    match parseCalcExn "example" "1.0 + 42.0 * sin 3.0"
    with File {e = example} in

-/


mexpr


-- Test code

use Complete in

let emptyEnv = mapEmpty cmpString in

let add_ = lam l. lam r.
  AddExpr{info = NoInfo (), left = l, right = r} in
let sub_ = lam l. lam r.
  SubExpr{info = NoInfo (), left = l, right = r} in
let num_ = lam v.
  NumExpr{info = NoInfo (), val = {v = v, i = NoInfo ()}} in

let example = add_ (num_ 2.0) (num_ 3.0) in

utest exprToFloat (eval emptyEnv example) with addf 2.0 3.0 in

printLn (toString (eval emptyEnv example));
printLn (toString example)
