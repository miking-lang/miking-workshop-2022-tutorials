-- Includes from the standard library
include "common.mc"
include "string.mc"


-- Includes from the local directory
include "./lib/ode.mc"
include "./tutorial1-mlang.mc"

/-

  The purpose of this tutorial is for you to combine what you have done and
  learned in previous tutorials to create a small Domain Specific Language (DSL)
  for modeling and solving Ordinary Differential Equations (ODEs).

  This tutorial is divided into several tasks, with somewhat rising complexity.

-/


-- Internal interface

lang FloatConversion = NumExprAst
  -- Converts an expression to a float. If `Expr` is something besides `NumExp`
  -- an error will occur.
  sem exprToFloat : Expr -> Float
  sem exprToFloat =
  | NumExpr r -> r.val.v
  | _ -> error "exprToFloat: undefined conversion"


  -- Converts a float into a `NumExpr`.
  sem floatToExpr : Float -> Expr
  sem floatToExpr =
  | v -> NumExpr { info = NoInfo (), val = { v = v, i = NoInfo ()} }
end

-- External interface (defined in ./lib/ode.mc)

lang ODEHelpers = ODEHelpersImpl

  /-
     Encodes a system of first-order explicit Ordinary Differential Equations
     (ODE) and used by the external ODE solver. The key in the map corresponds
     to the differentiated variable, while the value corresponds to the
     right-hand side of the differential equation. I.e., a function of all
     dependent variables and their values. The values of this map is of type
     `Expr`, but you can assume that these will always be `NumExpr`
     constructs. Consequently, you must ensure that the returned values of these
     functions are a `NumExpr` construct.

     Example:
     The differential equation
     x' = y
     y' = x

     Would be encoded as a map:
     {
       "x" => lam vars. mapFindExn "y" vars,
       "y" => lam vars. mapFindExn "x" vars
     }
  -/
  type Equations = Map String (Map String Expr -> Expr)

end


/-
  Task A ---------------------------------------------------------------------

  Change ast.syn to recognize systems of explicit first-order ODEs
  with the following concrete syntax:

  <eq> ::= x' = <expr>
  <file> ::= model <eq>{; <eq>}

  where x are identifier names, <expr> are expressions from tutorial 1, {...}
  denotes zero or more repetitions, and the starting production is <file>. You
  can find examples programs for this DSL in the `models` folder.

  In this DSL, free variables denotes dependent variables in the ODE model and
  x' denotes the derivative of x with respect to the independent variable. Note
  that x' may only appear on left-left hand side of an equality. For example,
  the ODE

  x'(t) = dx(t)
  dx(t) = -(dx(t) + x(t))

  would be encoded in our DSL as

  model
  x' = dx;
  dx' = 0. - (dx + x)

  At the end of this file is the entry point of the executable resulting from
  compiling this file. Please have a look there before starting solve this task.

  Note that you will need to usa another constructor for variables than
  `VarExpr` in `ast.syn` to avoid accidental shadowing of `VarExpr` as it is
  defined in `lib/lambda.mc`. You can for example add the production:

  prod FreeVar: Expr = name:LIdent

  to `ast.syn` and then transform all occurances of `FreeVar` after parsing to
  `VarExpr` in order to re-use the langauge fragments from `lib/lambda.mc`.

-/


/-
  Task B ---------------------------------------------------------------------

  Extend the `ODE` language fragment (below) with a semantic function
  `buildEquations` that takes as input your ODE AST from task A and
  produces a value of type `Equations` as specified in the language
  fragment `ODEHelpers` above.

  Again, at the end of this file is the entry point of the executable resulting
  from compiling this file.

-/


/-
  Task C ---------------------------------------------------------------------

  Extend the ODE DSL with a language construct to specify initial
  values. I.e. the concrete syntax is extended to:

  <eq> ::= x' = <expr>
  <init> :: = init x r
  <file> ::= {<init>} model <eq>{; <eq>}

  where r are real numbers.

  Example program:

  init x 1.
  model
  x' = dx;
  dx' = 0. - (dx + x)

-/


/-
  Task D ---------------------------------------------------------------------

  Extend the `ODE` language fragment with a static analysis of the ODE model
  which should check that:

  1. Only free variables x appear differentiated (x').
  2. All free variables appear differentiated exactly once.

-/


lang ODE = ODEHelpers + FloatConversion end


mexpr

use ODE in

-- Hard coded ODE for illustration purposes.
let eqn =
  mapFromSeq cmpString [
    ("t"
    ,lam vars : Map String Expr. floatToExpr 1.),
    ("x"
    ,lam vars : Map String Expr.
      let dx = exprToFloat (mapFindExn "dx" vars) in
      floatToExpr dx),
    ("dx"
    ,lam vars : Map String Expr.
      let t = exprToFloat (mapFindExn "t" vars) in
      let dx = exprToFloat (mapFindExn "dx" vars) in
      let x = exprToFloat (mapFindExn "x" vars) in
      floatToExpr (addf (negf (addf dx x)) (sin t)))
  ]
in

match argv with [prog] ++ args then
  let usage = join ["USAGE: ", prog, " ", odeUsage] in
  match args with [filename, outfile] ++ _ then
    -- For Task A ------------------------------------------------------------
    -- Implement the ODE concrete syntax and uncomment the lines below to
    -- parse the ODE model given as the first argument.
    -- let fileAst =
    --   if fileExists filename then
    --     let content = readFile filename in
    --     parseCalcExn filename content
    --   else
    --     printErrorLn (errMsg (join ["file ", filename, " not found"])); exit 1
    -- in

    -- For Task B ------------------------------------------------------------
    -- implement `buildEquations` and uncomment the line below to
    -- transform `fileAst` into `Equations`.
    -- let eqn = buildEquations fileAst in

    -- For simplicity we hardcode the solution interval.
    let interval = 20. in

    -- In the A-B task we assume that all dependent variables have zero as their
    -- initial value. In Task C you should replace this with the initial values
    -- specified in the model.
    let initialValues = mapEmpty cmpString in
    -- let initialValues = buildInitialVals fileAst in

    -- Here we pass `eqn` and `initialValues` to the IVP solver backend and the
    -- resulting solution trace is then printed to an html file in the same
    -- folder, displaying the solution trace, on success.
    switch ivpSolve eqn interval initialValues
      case Right sol then odeWriteSolution outfile sol
      case Left msg then printErrorLn msg; exit 1
    end
  else printLn usage; exit 1
else error "impossible"
