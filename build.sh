#!/usr/bin/env bash

export GRAPHICAL=ON
bash travis-build-opencv

export PKG_CONFIG_PATH=$HOME/usr/lib/pkgconfig:$(pkg-config --variable pc_path pkg-config)
export INCLUDE_PATH=$HOME/usr/include:${INCLUDE_PATH}
export LD_LIBRARY_PATH=$HOME/usr/lib:${LD_LIBRARY_PATH}

stack build && stack install && stack test
stack haddock
