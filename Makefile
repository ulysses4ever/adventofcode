DEPS := $(shell cat deps.txt)
all: build

build:
	ghc -threaded Main.hs
run-%:
	./Main $*
deps:
	cabal install --force-reinstall --enable-library-profiling --lib --package-env . $(DEPS)
inp-sample-%:
	cp Y2021/input/day-$*.sample Y2021/input/day-$*.txt
inp-full-%:
	cp Y2021/input/day-$*.full Y2021/input/day-$*.txt
shell:
	nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [$(DEPS)])" -p blas lapack
