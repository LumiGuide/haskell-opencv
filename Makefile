.PHONY: nothing
nothing: ;

.PHONY: shell
shell:
	nix-shell

.PHONY: build
build:
	cabal configure --enable-tests --flags="cuda" && \
	cabal build

.PHONY: clean
clean:
	cabal clean && \
	find doc/generated -name "*.png" -type f -delete && \
	find src           -name "*.cpp" -type f -delete

.PHONY: test
test:
	cabal configure --enable-tests --flags="cuda" && \
	cabal build && \
	./dist/build/doc-images-opencv/doc-images-opencv && \
	./dist/build/test-opencv/test-opencv

.PHONY: doc
doc: doc/color_conversions.png
	cabal configure --enable-tests --flags="cuda" && \
	cabal build && \
	./dist/build/doc-images-opencv/doc-images-opencv && \
	cabal haddock --hyperlink-source

.PHONY: update-git-haddock
update-git-haddock: doc/color_conversions.png
	git checkout master && \
	cabal configure --enable-tests --enable-coverage --flags="cuda" && \
	cabal build && \
	cabal test && \
	cabal haddock --hyperlink-source && \
	git checkout gh-pages && \
	git pull && \
	git rm -r doc && rm -rf doc && mv ./dist/doc/html/opencv doc && \
	git add doc && \
	git rm -r hpc && rm -rf hpc && mv ./dist/hpc/vanilla/html/opencv-0.0.0 hpc && \
	git add hpc && \
	git commit -m 'updated haddock documentation' && \
	git push && \
	git checkout master

doc/color_conversions.png: doc/color_conversions.dot
	dot $(<) -Tpng -Gsize=40 > $(@)
