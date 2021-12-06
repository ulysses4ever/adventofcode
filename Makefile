DEPS := $(shell cat deps.txt)
all: build

build:
	ghc Main.hs
run-%:
	./Main $*
deps:
	cabal install --lib --package-env . $(DEPS)
