include "mexpr/info.mc"


-- Base fragment

lang CalcBaseAst
  syn Expr =
end


-- Ast fragments for each kind of Expr

lang NumExprAst = CalcBaseAst
  syn Expr =
  | NumExpr {info : Info, val : {v:Float, i:Info}}
end

lang AddExprAst = CalcBaseAst
  syn Expr =
  | AddExpr {info : Info, left : Expr, right : Expr}
end

lang SubExprAst = CalcBaseAst
  syn Expr =
  | SubExpr {info : Info, left : Expr, right : Expr}
end

lang MulExprAst = CalcBaseAst
  syn Expr =
  | MulExpr {info : Info, left : Expr, right : Expr}
end

lang DivExprAst = CalcBaseAst
  syn Expr =
  | DivExpr {info : Info, left : Expr, right : Expr}
end


-- The full, composed language

lang CalcAst = NumExprAst + AddExprAst + SubExprAst + MulExprAst + DivExprAst
end
