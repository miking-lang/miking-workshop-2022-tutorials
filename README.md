# Build and run instructions

This tutorial is set up with a `Makefile` that can build and run all
tasks. This makefile is also set up to be intelligent in the sense
that it will use Docker if possible (since we suggest that for
ease-of-installation reasons), and otherwise fall back to running
outside of Docker.

Each task has a `make` target for building and running, and one target
for building and running automatically as soon as files changed. These
targets are listed under the relevant heading below, and will also be
mentioned during the tutorial sessions.

# Tutorials

## Tutorial 1, MExpr and MLang

This tutorial is divide into two parts, one that covers MExpr and one
that covers MLang.

### Part 1, MExpr

All tasks can be found in `tutorial1-mexpr.mc`. Relevant `make`
targets:
- `make mexpr` builds and runs `tutorial1-mexpr.mc`
- `make watch-mexpr` automatically runs `make mexpr` every time files
  change.

### Part 2, MLang

All tasks can be found in `tutorial1-mlang.mc`. Relevant `make`
targets:

- `make mlang` builds and runs `tutorial1-mlang.mc`
- `make watch-mlang` automatically runs `make mlang` every time files
  change.

Other relevant files:
- `tutorial1-mexpr.mc` (`include`d in `tutorial1-mlang.mc`)
- `ast.mc`, defines the AST we manipulate in the tasks
- `lib/lambda.mc`, contains language fragments for the lambda-calculus. You
  should not have to edit this file but you can have a look if you are curious

## Tutorial 2, concrete syntax and ODE DSL

The first task is Task C at the end of `tutorial1-mlang.mc`. This task
uses the same `make` targets as Tutorial 1, Part 2.

Remaining tasks can be found in `tutorial2-ode.mc`. Relevant `make`
targets:

- `make ode` builds and runs `tutorial2-ode.mc` on all models in the
  `models/` directory.
- `make watch-ode` automatically runs `make ode` every time files
  change.

Relevant files and folders:
- `tutorial1-mexpr.mc` and `tutorial1-mlang.mc`, since some work is reused.
- `ast.syn`, this file will describe your syntax
- `ast-gen.mc`, this file is generated from your specification in `ast.syn`
- the folder `models` contains ODE models (`.ode` files) and the
  solution traces for these models (`.html` files) as computed by your
  ODE DSL. To view a solution trace simply open it in a
  web-browser. You need to refresh your browser if you rebuild/re-run
  the `tutorial2-ode.mc` file. Note that before you have solved Task B all
  solution traces will be for a hard-coded model that we include to
  provide some output, even before the backend is complete.
- `lib/ode.mc`, interfaces the ODE solver. You should not have to edit this file
  but you can have a look if you are curious
- `lib/lambda.mc`, contains language fragments for the lambda-calculus. You
  should not have to edit this file but you can have a look if you are curious
