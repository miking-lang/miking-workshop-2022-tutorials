include "mexpr/info.mc"
include "error.mc"
include "map.mc"

-- Ast fragments

lang LambdaAst
  syn Expr =
  | LamExpr {info : Info, name : {v : String, i : Info}, body : Expr}
end

lang AppAst
  syn Expr =
  | AppExpr {info : Info, left : Expr, right : Expr}
end

lang VarAst
  syn Expr =
  | VarExpr {info : Info, name : {v:String, i:Info}}
end

-- Helpers to construct ASTs a bit more concisely

let lam_ = lam name. lam body. use LambdaAst in
  LamExpr{info = NoInfo (), name = {v = name, i = NoInfo ()}, body = body}
let app_ = lam l. lam r. use AppAst in
  AppExpr{info = NoInfo (), left = l, right = r}
let var_ = lam name. use VarAst in
  VarExpr{info = NoInfo (), name = {v = name, i = NoInfo ()}}

-- Eval fragments

lang LambdaEval = LambdaAst
  syn Expr =
  | Closure {name : String, body : Expr, env : Map String Expr}

  sem eval env =
  | LamExpr x -> Closure {name = x.name.v, body = x.body, env = env}
end

lang AppEval = AppAst + LambdaEval
  sem eval env =
  | AppExpr x ->
    match eval env x.left with Closure f in
    let arg = eval env x.right in
    eval (mapInsert f.name arg f.env) f.body
end

lang VarEval = VarAst
  sem eval env =
  | VarExpr x ->
    match mapLookup x.name.v env with Some val
    then val
    else errorSingle [x.info] (concat "Unbound variable " x.name.v)
end


-- ToString fragments

lang LambdaToString = LambdaAst
  sem toString =
  | LamExpr x -> join ["(lam ", x.name.v, ". ", toString x.body, ")"]
end

lang AppToString = AppAst
  sem toString =
  | AppExpr x -> join ["(", toString x.left, " ", toString x.right, ")"]
end

lang VarToString = VarAst
  sem toString =
  | VarExpr x -> x.name.v
end

lang ClosureToString = LambdaEval
  sem toString =
  | Closure x -> join ["(closure ", x.name, ". ", toString x.body, ")"]
end


-- Composed fragments

lang LambdaCalcAst = LambdaAst + AppAst + VarAst
end

lang LambdaCalcEval = LambdaEval + AppEval + VarEval
end

lang LambdaCalcToString = LambdaToString + AppToString + VarToString + ClosureToString
end
