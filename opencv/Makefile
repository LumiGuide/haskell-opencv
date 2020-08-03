.PHONY: nothing
nothing: ;

.PHONY: shell
shell:
	nix-shell

.PHONY: build
build:
	cabal v1-configure --enable-tests && \
	cabal v1-build

.PHONY: clean
clean:
	cabal v1-clean && \
	find doc/generated -name "*.png" -o -name "*.gif" -type f -delete

.PHONY: test
test:
	cabal v1-configure --enable-tests && \
	cabal v1-build && \
	./dist/build/doc-images-opencv/doc-images-opencv && \
	./dist/build/test-opencv/test-opencv

.PHONY: coverage
coverage:
	cabal v1-configure --enable-tests --enable-coverage && \
	cabal v1-test && \
	chromium dist/hpc/vanilla/html/opencv-0.0.0/hpc_index.html

.PHONY: doc
doc: doc/color_conversions.png
	cabal v1-configure --enable-tests && \
	cabal v1-build --ghc-option=-O0 && \
	./dist/build/doc-images-opencv/doc-images-opencv && \
	cabal v1-haddock --hyperlink-source

.PHONY: update-git-haddock
update-git-haddock: doc/color_conversions.png
	git checkout master && \
	cabal v1-configure --enable-tests --enable-coverage && \
	cabal v1-build && \
	cabal v1-test && \
	cabal v1-haddock --hyperlink-source && \
	git checkout gh-pages && \
	git pull && \
	git rm -r ../doc && rm -rf ../doc && mv ./dist/doc/html/opencv ../doc && \
	git add ../doc && \
	git rm -r ../hpc && rm -rf ../hpc && mv ./dist/hpc/vanilla/html/opencv-0.0.2.1 ../hpc && \
	git add ../hpc && \
	git commit -m 'updated haddock documentation' && \
	git push && \
	git checkout master

doc/color_conversions.png: doc/color_conversions.dot
	dot $(<) -Tpng -Gsize=40 > $(@)
