-- Includes from the standard library
include "math.mc"
include "common.mc"
include "seq.mc"
include "map.mc"


/-

  The purpose of this tutorial is for you to familiarize yourself with
  MExpr, which is MCore without MLang, i.e. no language fragments. We
  will however use top-level bindings.

  This tutorial is divided into two main tasks with somewhat rising
  complexity. You can do them in any order you want but you will re-use your
  solution to Task B in later tutorials. Task G contains suggestions of
  additional tasks if you finish the other tasks quickly.

-/


/-

  The idea behind task A and B is for you to familiarize yourself with
  [recursive] functions, common intrinsics, control-flow, and utests.

-/


/-
  Task A --------------------------------------------------------------------

  Provide an implementation for the function `fact` below.

  TIP: Useful intrinsics:
  - Float multiplication is `mulf`.
  - Integer addition and subtraction are `addi`, `subi`, respectively.
  - Integer equality, greater, and less-than comparsion are `eqi`, `gti`, and
    `lti` respectively.
  - `or a b` is true if at least one of `a` or `b` is true.
  - `error STRING` exits with an error and prints STRING.

-/


-- `fact n` computes the factorial !n, where we assume n >= 0.
let fact : Int -> Int = lam n : Int. never -- You should replace `never` with your implementation.

-- START LIVE CODING SOLUTION
-- We first ensure that our unit tests fails on an incorrect output.
let fact : Int -> Int = lam n : Int. 0

-- We then handle the base case n=0
let fact : Int -> Int = lam n : Int.
  if eqi n 0 then 1
  else 0

-- We can also assert n >= 0
let fact : Int -> Int = lam n : Int.
  if lti n 0 then error "Undefined"
  else if eqi n 0 then 1
  else 0

-- For all other n we call `fact` recursivly with the predecessor of n.
recursive let fact : Int -> Int = lam n : Int.
  if lti n 0 then error "Undefined"
  else if eqi n 0 then 1
  else muli n (fact (subi n 1))
end    -- This `end` keyword marks the end of mutually recursive top-level lets.

-- All unit tests should now pass.
-- END LIVE CODING SOLUTION

-- Some tests for `fact`, add to these if you like. Uncomment these to add these
-- tests to the executable.
utest fact 0 with 1
utest fact 1 with 1
utest fact 2 with 2
utest fact 3 with 6
utest fact 10 with 3628800


/-
  Task B --------------------------------------------------------------------

  Provide an implementation for the function `pow` below.

  TIP: Useful intrinsics:
  - Float multiplication and division are `mulf` and `divf`, respectively.
  - Integer addition and subtraction are `addi`, `subi`, respectively.
  - Integer equality, greater, and less-than comparsion are `eqi`, `gti`, and
    `lti` respectively.
  - Float equality comparison is `eqf`.
  - `or a b` is true if at least one of `a` or `b` is true.
  - `error STRING` exits with an error and prints STRING.

-/

-- `pow x n` computes `x` to the power of `n`.
let pow : Float -> Int -> Float = never

-- Some tests for `pow`, you can add to these if you like. Keep in mind that
-- it can sometimes be tricky to compare floating point numbers.
-- utest pow 2. 2 with 4.
-- utest pow 10. 1 with 10.
-- utest pow 10. 0 with 1.
-- utest pow 0. 1000 with 0.
-- utest pow 10. (negi 1) with 0.1
-- utest pow 2. (negi 3) with 0.125 using eqfApprox 0.01


/-

  The idea behind tasks C-F is for you to familiarize yourself with
  constructors, tuples, records, and pattern matching. The tasks involves
  defining a binary search tree, where the nodes can hold key-value pairs as
  data. We then define two operations on this tree, `treeInsert` and
  `treeLookup` that adds and retrieves data from a tree, respectively.

-/


/-
  Task C --------------------------------------------------------------------

  Define constructors for the polymorphic type `Tree a` below so that it can
  encode a binary search tree, where the nodes holds a key-value of polymorphic
  type (Int, a) as data, and where leaves holds no data.

-/

-- A binary tree can either be a leaf or a node. Here `a` is a type variable.
type Tree a

-- START LIVE CODING SOLUTION
-- We can think of a constructor as a function without a definition that takes
-- exacly one argument. We define constructors with the `con` keyword.

-- Our tree is polymorphic so we will also have to declare a type quantifier
-- using the `all` keyword.

-- Our leaves will not hold any data so we give their argument the
-- type of an empty record `{}` (or equivalently an empty tuple `()`).
con Leaf : all a. {} -> Tree a

-- Our nodes will contain a left and right tree in addition to a key-value pair.
-- We can conveniently encode this data with records and tuples.
con Node : all a. { left : Tree a, right : Tree a, data : (Int, a) } -> Tree a
-- END LIVE CODING SOLUTION

/-
  Task D --------------------------------------------------------------------

  Provide a definition for `treeEmpty` below.

-/

-- Constructs an empty tree.
let treeEmpty : all a. Tree a = never

-- START LIVE CODING SOLUTION
-- An empty tree is here simply a leaf
let treeEmpty : all a. Tree a = Leaf {}
-- END LIVE CODING SOLUTION


/-
  Task E --------------------------------------------------------------------

  Provide a definition for `treeInsert` below.

-/

-- `treeInsert key val tree` inserts a `key`, `value` pair into `tree`. If the
-- `key` is already in `tree`, the value associated with `key` is updated to
-- `value`.
let treeInsert : all a. Int -> a -> Tree a -> Tree a =
  lam key. lam val. lam tree. never

-- START LIVE CODING SOLUTION
-- We de-construct constructors with the `match e then e else e` expression. The
-- `never` keyword dictates a part in the code that we should never reach during
-- runtime in a well-typed program.
let treeInsert : all a. Int -> a -> Tree a -> Tree a =
  lam key. lam val. lam tree.
    -- The wildcard pattern `_` matches anything but we ignore its content.
    match tree with Leaf _ then never
    else match tree with Node _ then never
    else never

-- If `tree` is a leaf we have found a place where we can insert a new node.
let treeInsert : all a. Int -> a -> Tree a -> Tree a =
  lam key. lam val. lam tree.
    match tree with Leaf _ then
      Node { left = Leaf {}, right = Leaf {}, val = (key, val) }
    else match tree with Node _ then never
    else never

-- If `tree` is a node we either update its value or traverse down its left or
-- right sub-tree.
recursive let treeInsert : all a. Int -> a -> Tree a -> Tree a =
  lam key. lam val. lam tree.
    match tree with Leaf _ then
      Node { left = Leaf {}, right = Leaf {}, data = (key, val) }
    -- We use an and pattern `&` to both bind the record of this node to a
    -- variable `r` and to match on its key.
    else match tree with Node (r & { data = (nodeKey, _) }) then
      if eqi key nodeKey then
        -- We update records with the `with` keyword (note that this is not a
        -- side-effect).
        Node { r with data = (key, val) }
      else if lti key nodeKey then
        Node { r with left = treeInsert key val r.left }
      else
        Node { r with right = treeInsert key val r.right }
    else never
end
-- END LIVE CODING SOLUTION


/-
  Task F --------------------------------------------------------------------

  Provide a definition for `treeLookup` below.

-/

-- `treeLookup key tree` returns `Some val` if `val` is associated with `key` in
-- `tree` or `None {}` if `key` does not exist in `tree`.
let treeLookup = lam key. lam tree. never


let _testTree =
  foldl (lam tree. lam x. match x with (key, val) in treeInsert key val tree)
  treeEmpty
  [(0, "0"), (2, "2"), (1, "1"), (3, "2"), (3, "3"), (4, "4")]

-- utest treeLookup 0 _testTree with Some "0"
-- utest treeLookup 1 _testTree with Some "1"
-- utest treeLookup 2 _testTree with Some "2"
-- utest treeLookup 3 _testTree with Some "3"
-- utest treeLookup 4 _testTree with Some "4"
-- utest treeLookup 5 _testTree with None {}
-- utest treeLookup 6 _testTree with None {}
-- utest treeLookup (negi 1) _testTree with None {}


/-
  Task G --------------------------------------------------------------------
  Suggested extra tasks:

  - Change `fact` and `pow` so that these are total functions (i.e. don't apply
    `error`) by for exmple using the option type `Option a` which has the
    constructors:

    `Some : all a. a -> Option a` and `None : all a. {} -> Option a`.

    You will have to define a custom equality function for the utests and use it
    with the `using` keyword.

  - Add an operation `treeRemove key tree` which removes the key-value pair
    associated with `key` from `tree`.

-/
