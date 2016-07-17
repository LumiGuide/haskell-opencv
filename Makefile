.PHONY: shell
shell:
	nix-shell

.PHONY: build
build:
	cabal configure --enable-tests && cabal build

.PHONY: test
test:
	cabal configure --enable-tests && cabal build && ./dist/build/opencv-doc-images/opencv-doc-images && ./dist/build/test-opencv/test-opencv

.PHONY: doc
doc:
	cabal configure --enable-tests && cabal build && ./dist/build/opencv-doc-images/opencv-doc-images && cabal haddock --hyperlink-source
