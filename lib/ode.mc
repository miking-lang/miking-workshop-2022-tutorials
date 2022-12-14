include "common.mc"
include "string.mc"
include "map.mc"
include "option.mc"
include "either.mc"
include "sundials/cvode.mc"

let printErrorLn = lam s. printError s; printError "\n"; flushStderr ()
let errMsg = lam msg. join ["Error: ", msg]
let warnMsg = lam msg. join ["Warning: ", msg]

let solutionHeaderString : [String] -> String = strJoin ","

let solutionStepToString
  : [String] -> Map String Float -> String
  = lam header. lam step.
    let valStrs = foldl
      (lam acc. lam var.
        optionMapOr acc
          (lam val. snoc acc (float2string val)) (mapLookup var step))
      []
      header
    in strJoin "," valStrs

let _htmlTemplatePre = strJoin "\n"
  [ "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/d3/7.7.0/d3.min.js\"></script>"
  , "<script src=\"https://cdn.jsdelivr.net/npm/chart.js@4.0.1\"></script>"
  , "<div><canvas id=chart></canvas></canvas>"
  , ""
  , "<script>"
  , "const data = d3.csvParse(\""
  ]

let _htmlTemplatePost = strJoin "\n"
  ["\");"
  , "  const ctx = document.getElementById('chart');"
  , "  let sets = {};"
  , "  for (var p in data[0]) {"
  , "      sets[p] = [];"
  , "  }"
  , "  data.forEach(row => {"
  , "      for (var p in row) {"
  , "          sets[p].push(+row[p]);"
  , "      }"
  , "  });"
  , "  let datasets = [];"
  , "  for (var set in sets) {"
  , "      datasets.push({label: set, data: sets[set]});"
  , "  }"
  , "  new Chart(ctx, {"
  , "    type: 'line',"
  , "    data: {"
  , "      labels: Array.from(Array(data.length).keys()),"
  , "      datasets: datasets"
  , "    },"
  , "  });"
  , "</script>"
  ]

-- ** CALLBACK FUNCTION ARG
type ODEFnArg = {
  t : Float,                    -- free variable t
  xs : [Float],                 -- states x(t) ∈ ℝ^nx
  us : [Float],                 -- inputs u(t) ∈ ℝ^nu
  ps : [Float]                  -- parameters p ∈ ℝ^np
}

-- ** INTEGRATION STEP FUNCTION
-- Fourth order explicit Runge Kutta (RK4) ODEs integration step.
-- `f`    : function that computes x'(t) ∈ ℝ^nx
-- `xidxs` : index vector of size nx
-- `h`    : step-size of the integration
-- `t`    : value of t
-- `xs`   : values of the states x(t) ∈ ℝ^nx
-- `us`   : inputs u ∈ ℝ^nu
-- `ps`   : parameters p ∈ ℝ^np
let odeSolverRK4Step :
  (ODEFnArg -> [Float])
  -> [Int]
  -> Float
  -> Float
  -> [Float]
  -> [Float]
  -> [Float]
  -> [Float] =
  lam f. lam xidxs. lam h. lam t. lam xs. lam us. lam ps.
    let h2 = divf h 2. in
    let t2 = addf t h2 in
    let th = addf t h in
    let k1s = f { t = t, xs = xs, us = us, ps = ps } in
    let tmps = map2 (lam x. lam k. addf x (mulf h2 k)) xs k1s in
    let k2s = f { t = t2, xs = tmps, us = us, ps = ps} in
    let tmps = map2 (lam x. lam k. addf x (mulf h2 k)) xs k2s in
    let k3s = f { t = t2, xs = tmps, us = us, ps = ps } in
    let tmps = map2 (lam x. lam k. addf x (mulf h k)) xs k3s in
    let k4s = f { t = th, xs = tmps, us = us, ps = ps } in
    map
      (lam i.
        let k1 = get k1s i in
        let k2 = mulf 2. (get k2s i) in
        let k3 = mulf 2. (get k3s i) in
        let k4 = get k4s i in
        let x = get xs i in
          addf x (mulf h (divf (addf k1 (addf k2 (addf k3 k4))) 6.)))
      xidxs


lang ODEHelpersImpl
  syn Expr =


  type EquationsImpl = Map String (Map String Expr -> Expr)


  sem exprToFloat : Expr -> Float
  sem floatToExpr : Float -> Expr


  sem _wrapRHS
  : [String] ->
    [Map String Expr -> Expr] ->
    (Tensor[Float] -> Tensor[Float] -> ())
  sem _wrapRHS vars =
  | rhs ->
    lam y : Tensor[Float]. lam dy : Tensor[Float].
      let state =
        mapFromSeq cmpString
          (mapi (lam i. lam var. (var, floatToExpr (tensorLinearGetExn y i))) vars)
      in
      let lhs = map (lam rh. exprToFloat (rh state)) rhs in
      iteri (lam i. lam v. tensorLinearSetExn dy i v) lhs

  syn Method =
  | RK4 {}
  | BDF {}

  /-
     `ivpSolve eqn interval initialValues` takes an ODE, an interval, and inital
     values, and produces a solution trace.

     PARAMETERS:
      - `eqn` : ODE equations encoded as described for `Equations`.
      - `interval` : encodes the interval [t0, tf] where to solve the ODE.
      - `initialValues` : A map from dependent variables to their values at time
                          t0. If a dependent variable is not present in this map
                          it will get the default initial value 0.

     RETURNS: Either `Left sol`, where `sol` is a sequence of mappings between
      dependent variables and their solution for each time-step in the interval
      t0 to tf, or `Right msg` which indicates an solver error with the error
      messages `msg`.
  -/
  sem ivpSolve
  : EquationsImpl ->
    Float ->
    Map String Float ->
    Method ->
    Either String [Map String Float]
  sem ivpSolve eqs interval initialValues =
  | RK4 _ -> ivpSolveRK4 eqs interval initialValues
  -- | BDF _ -> ivpSolveCVODE eqs interval initialValues

  -- sem ivpSolveCVODE
  -- : EquationsImpl ->
  --   Float ->
  --   Map String Float ->
  --   Either String [Map String Float]
  -- sem ivpSolveCVODE eqs interval =
  -- | initialState ->
  --   match unzip (mapBindings eqs) with (vars, rhs) in
  --   let rhs = _wrapRHS vars rhs in
  --   let n = length vars in
  --   let y = tensorCreateCArrayFloat [n] (lam. 0.) in
  --   iteri
  --     (lam i. lam var.
  --       let v = optionGetOrElse
  --         (lam.
  --           printErrorLn
  --             (warnMsg
  --               (join ["Unspecified initial value, setting ", var, "=0. (Don't worry about this until Task C)"]));
  --           0.)
  --         (mapLookup var initialState)
  --       in
  --       tensorLinearSetExn y i v)
  --     vars;
  --   let initialStateVars = mapKeys initialState in
  --   iter
  --     (lam var.
  --       if mapMem var eqs then ()
  --       else
  --         printErrorLn
  --           (warnMsg
  --             (join ["Initial state ", var, " does not appear in model"])))
  --     initialStateVars;
  --   let v = nvectorSerialWrap y in
  --   let m = sundialsMatrixDense n in
  --   let lsolver = cvodeDlsSolver (cvodeDlsDense v m) in
  --   let t0 = 0. in
  --   let tend = interval in
  --   let dt = 0.1 in
  --   let s = cvodeInit {
  --     lmm      = cvodeLMMBDF,
  --     tol      = cvodeDefaultTolerances,
  --     lsolver  = lsolver,
  --     rhs      = lam. rhs,
  --     t0       = t0,
  --     y0       = v
  --   } in
  --   cvodeSetStopTime s interval;
  --   recursive let recur = lam sol. lam t.
  --     let step = mapFromSeq cmpString
  --       (mapi (lam i. lam var. (var, tensorLinearGetExn y i)) vars)
  --     in
  --     let sol = snoc sol step in
  --     switch cvodeSolveNormal { s = s, tend = addf t dt, y = v }
  --       case (tend, CVODESuccess _) then
  --         recur sol tend
  --       case (_, CVODEStopTimeReached _) then
  --         Right sol
  --       case (_, CVODERootsFound _) then
  --         Left (errMsg "Impossible, we should not find any roots")
  --       case (_, CVODEError err) then
  --         switch err
  --           case
  --             CVODEIllInput _ |
  --             CVODELinearInitFailure _ |
  --             CVODELinearSetupFailure _
  --           then
  --             Left (errMsg "Solver failed to initialize")
  --           case
  --             CVODETooClose _ |
  --             CVODETooMuchWork _ |
  --             CVODETooMuchAccuracy _ |
  --             CVODEErrFailure _ |
  --             CVODEConvergenceFailure _ |
  --             CVODELinearSolveFailure _
  --           then
  --             Left (errMsg "Solver failed to converge to a solution")
  --           case
  --             CVODELinearSolveFailure _ |
  --             CVODERhsFuncFailure _ |
  --             CVODEFirstRhsFuncFailure _ |
  --             CVODERepeatedRhsFuncFailure _ |
  --             CVODEUnrecoverableRhsFuncFailure _
  --           then
  --             Left (errMsg "Solver failed to evaluate equations")
  --           case _ then
  --             Left (errMsg "Unspecified solver error")
  --         end
  --     end
  --   in recur [] t0

  sem ivpSolveRK4
  : EquationsImpl ->
    Float ->
    Map String Float ->
    Either String [Map String Float]
  sem ivpSolveRK4 eqs interval =
  | initialState ->
    match unzip (mapBindings eqs) with (vars, rhs) in
    let f = lam arg.
      let state =
        mapFromSeq cmpString
          (zipWith (lam var. lam val. (var, floatToExpr val)) vars arg.xs)
      in
      map (lam rh. exprToFloat (rh state)) rhs
    in
    let xidxs = create (length vars) (lam i. i) in
    let h = 0.1 in
    let xs =
      map
        (lam var.
          let v = optionGetOrElse
            (lam.
              printErrorLn
                (warnMsg
                  (join ["Unspecified initial value, setting ", var, "=0. (Don't worry about this until Task C)"]));
              0.)
            (mapLookup var initialState)
          in
          v)
        vars
    in
    let ts =
      unfoldr
        (lam t. if geqf t interval then None {} else Some (t, addf t h)) 0.
    in
    let xss =
      foldl
        (lam xss. lam t.
          let xs = head xss in
          let nextxs = odeSolverRK4Step f xidxs h t xs [] [] in
          cons nextxs xss)
        [xs]
        ts
    in
    let sol = map
      (lam xs.
        mapFromSeq cmpString (zipWith (lam var. lam val. (var, val)) vars xs))
      xss
    in
    Right (reverse sol)


  /-
     `odeWriteSolution sol` prints the solution `sol` to stdout as comma seprated
     list, where the dependent variables names form the header and the rows
     corresponds the their values at each time-step.
  -/
  sem odeWriteSolution : String -> [Map String Float] -> ()
  sem odeWriteSolution filename =
  | sol ->
    if null sol then printErrorLn (warnMsg "odeWriteSolution: No solution")
    else
      let header = mapKeys (head sol) in
      let contents = strJoin "\\n" (cons (solutionHeaderString header) (map (solutionStepToString header) sol)) in
      writeFile filename (concat _htmlTemplatePre (concat contents _htmlTemplatePost))


  /-
     Read a seqences of strings on the form:
      ["interval", "name=value", "name=value", ...],
     where:
      - interval is a string representation of float, indicating an interval;
      - name=value name=value ... is a seqence of dependent variable names
        string representation of a float indicating variables inital value.

     RETURNS: Either, `Right r`, where `r` is a record of the parsed input, or
      `Left msg` if there is a parse error with error message `msg`.
  -/
  sem odeReadSolverConfig
    : [String] ->
      Either String {interval : Float, initialValues : Map String Float}
  sem odeReadSolverConfig =
  | xs ->
      let parseFloat = lam str.
        if stringIsFloat str then Right (string2float str)
        else if stringIsInt str then Right (int2float (string2int str))
        else Left (errMsg (join ["Parsing \"", str, "\" as float"]))
      in
      let parseInitialValue = lam str.
        match map strTrim (strSplit "=" str) with [x, val] ++ _ then
          eitherBindRight (parseFloat val) (lam val. Right (x, val))
        else Left (errMsg (join ["Parsing \"", str, "\" as initial value"]))
      in
      match xs with [x] ++ xs then
        eitherBindRight (parseFloat x)
          (lam interval.
            switch eitherPartition (map parseInitialValue xs)
              case ([e] ++ _, _) then Left e
              case (_, initialValues) then
                Right {
                  interval = interval,
                  initialValues = mapFromSeq cmpString initialValues
                }
            end)
      else Left (errMsg "Found no interval")
end

lang TestODEHelpersImpl = ODEHelpersImpl
  syn Expr =
  | NumExpr Float

  sem exprToFloat =
  | NumExpr val -> val
  | _ -> error "Undefined"

  sem floatToExpr =
  | val -> NumExpr val
end


let odeUsage = strJoin "\n" [
"INFILE OUTFILE INTERVAL NAME=VAL NAME=VAL NAME=VAL",
"\n",
"where INTERVAL is the ODE solution interval and NAME=VAL assigns",
"an inital value VAL to the dependent variable NAME"
]

mexpr

-- use TestODEHelpersImpl in

-- let eq = eitherEq
--   (lam x. lam y.
--     and
--       (eqf x.interval y.interval)
--       (eqSeq
--         (lam a. lam b. and (eqString a.0 b.0) (eqf a.1 b.1))
--         (mapToSeq x.initialValues) y.initialValues))
--   eqString
-- in

-- utest odeReadSolverConfig ["10", "x =1.", "y = 2.2", "z=03"]
-- with Right { interval = 10., initialValues = [("x", 1.), ("y", 2.2), ("z", 3.)]}
-- using eq in

-- utest odeReadSolverConfig ["10.1", "x =1.", "y = 2.2", "z=03"]
-- with Right { interval = 10.1, initialValues = [("x", 1.), ("y", 2.2), ("z", 3.)]}
-- using eq in

-- utest odeReadSolverConfig ["10."]
-- with Right { interval = 10., initialValues = []}
-- using eq in

-- utest odeReadSolverConfig []
-- with Left "Error: Found no interval"
-- using eq in

-- utest odeReadSolverConfig ["10A", "x =1.", "y = 2.2", "z=03"]
-- with Left "Error: Parsing \"10A\" as float"
-- using eq in

-- utest odeReadSolverConfig ["10", "x =", "y = 2.2", "z=03"]
-- with Left "Error: Parsing \"\" as float"
-- using eq in

-- utest odeReadSolverConfig ["10", "x", "y = 2.2", "z=03"]
-- with Left "Error: Parsing \"x\" as initial value"
-- using eq in

-- let step = mapFromSeq cmpString [("x", 1.), ("y", 2.2), ("z", 3.)] in
-- utest solutionHeaderString (mapKeys step) with "x,y,z" in

-- utest solutionStepToString (mapKeys step) step  with "1.,2.2,3." in

-- -- TESTS SOLUTION PRINTER
-- let sol = [
--   mapFromSeq cmpString [("x", 1.), ("y", 2.2), ("z", 3.)],
--   mapFromSeq cmpString [("x", 2.), ("y", 3.2), ("z", 4.)]
-- ] in

-- -- odeWriteSolution sol;
-- -- odeWriteSolution [];

-- -- TEST SOLVER BACKEND
-- let harmonicOscillator =
--   mapFromSeq cmpString [
--     ("x"
--     ,lam states : Map String Expr.
--       let dx = exprToFloat (mapFindExn "dx" states) in
--       NumExpr dx),
--     ("dx"
--     ,lam states : Map String Expr.
--       let dx = exprToFloat (mapFindExn "dx" states) in
--       let x = exprToFloat (mapFindExn "x" states) in
--       NumExpr (negf (addf dx x)))
--   ]
-- in

-- let initialState = mapFromSeq cmpString [("x", 1.), ("dx", 0.)] in
-- utest
--   match ivpSolveCVODE harmonicOscillator 10. initialState with Right _ then true
--   else false
-- with true in

-- -- -- Solves and prints solution
-- -- (switch ivpSolveCVODE harmonicOscillator 10. initialState
-- --   case Right sol then odeWriteSolution filename sol
-- --   case Left msg then printLn msg
-- -- end);

()
