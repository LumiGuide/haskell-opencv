.PHONY: nothing
nothing: ;

.PHONY: shell
shell:
	nix-shell

.PHONY: build
build:
	cabal configure --enable-tests && \
	cabal build

.PHONY: clean
clean:
	cabal clean && \
	find src           -name "*.cpp" -type f -delete

.PHONY: doc
doc:
	cabal configure --enable-tests && \
	cabal build && \
	cabal haddock --hyperlink-source
