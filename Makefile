# Automatically run all commands in docker if it and the miking image
# is installed
SHELL := $(shell ./lib/detect-docker.sh)

MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.PHONY: all
all: tutorial1-mexpr tutorial1-mlang ode

# Commands to run the tutorial parts in a simple way
.PHONY: mexpr mlang ode
mexpr: tutorial1-mexpr
	./tutorial1-mexpr
mlang: tutorial1-mlang
	./tutorial1-mlang
ode: $(addsuffix .html,$(basename $(wildcard models/*.ode)))


# Watch commands for each of the tutorial parts
.PHONY: watch-mexpr
watch-mexpr:
ifneq ($(strip $(shell which entr)),)
	find . "(" -name "*.mc" -o -name "*.syn" ")" -a ! -name "ast-gen.mc" | entr -rc make mexpr
else
	@echo "'watch' depends on 'entr' (https://eradman.com/entrproject/) being installed,"
	@echo "but it isn't on your PATH."
endif

.PHONY: watch-mlang
watch-mlang:
ifneq ($(strip $(shell which entr)),)
	find . "(" -name "*.mc" -o -name "*.syn" ")" -a ! -name "ast-gen.mc" | entr -rc make mlang
else
	@echo "'watch' depends on 'entr' (https://eradman.com/entrproject/) being installed,"
	@echo "but it isn't on your PATH."
endif

.PHONY: watch-ode
watch-ode:
ifneq ($(strip $(shell which entr)),)
	find . "(" -name "*.mc" -o -name "*.syn" ")" -a ! -name "ast-gen.mc" | entr -rc make ode
else
	@echo "'watch' depends on 'entr' (https://eradman.com/entrproject/) being installed,"
	@echo "but it isn't on your PATH."
endif

# Building each part of the tutorial
tutorial1-mexpr: tutorial1-mexpr.mc
	mi compile --test tutorial1-mexpr.mc --output $@
	chown --reference=Makefile $@

tutorial1-mlang: ast-gen.mc ast.mc tutorial1-mexpr.mc tutorial1-mlang.mc
	mi compile --test tutorial1-mlang.mc --output $@
	chown --reference=Makefile $@

tutorial2-ode: ast-gen.mc tutorial1-mexpr.mc tutorial1-mlang.mc tutorial2-ode.mc
	mi compile --test tutorial2-ode.mc --output $@
	chown --reference=Makefile $@

# Running the DSL from tutorial 2 to generate plots for each ODE
models/%.html: models/%.ode tutorial2-ode
	./tutorial2-ode $< $@
	chown --reference=Makefile $@

# Generating the parser and AST from the '.syn' file
ast-gen.mc: ast.syn
	mi syn ast.syn ast-gen.mc
	chown --reference=Makefile $@
