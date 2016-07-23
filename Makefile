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
	find doc/generated -name "*.png" -type f -delete && \
	find src           -name "*.cpp" -type f -delete

.PHONY: test
test:
	cabal configure --enable-tests && \
	cabal build && \
	./dist/build/opencv-doc-images/opencv-doc-images && \
	./dist/build/test-opencv/test-opencv

.PHONY: doc
doc:
	cabal configure --enable-tests && \
	cabal build && \
	./dist/build/opencv-doc-images/opencv-doc-images && \
	cabal haddock --hyperlink-source

.PHONY: update-git-haddock
update-git-haddock:
	git checkout master && \
	cabal configure --enable-tests && \
	cabal build && \
	./dist/build/opencv-doc-images/opencv-doc-images && \
	cabal haddock --hyperlink-source && \
	git stash save && \
	git checkout gh-pages && \
	git rm -r doc && mv ./dist/doc/html/opencv doc && \
	git add doc && \
	git commit -m 'updated haddock documentation' && \
	git push && \
	git checkout master && \
	git stash pop
