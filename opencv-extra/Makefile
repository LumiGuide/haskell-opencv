.PHONY: nothing
nothing: ;

.PHONY: shell
shell:
	nix-shell

.PHONY: build
build:
	cabal configure --enable-tests && \
	cabal build

.PHONY: test
test:
	cabal configure --enable-tests && \
	cabal build && \
	./dist/build/doc-images-opencv-extra/doc-images-opencv-extra

.PHONY: clean
clean:
	cabal clean && \
	find doc/generated -name "*.png" -type f -delete

.PHONY: doc
doc:
	cabal configure --enable-tests && \
	cabal build && \
	./dist/build/doc-images-opencv-extra/doc-images-opencv-extra && \
	cabal haddock --hyperlink-source
