Haskell OpenCV-3.1 binding
==========================

This is a Haskell library providing a binding to OpenCV-3.1. It binds directly
with the C++ API using the
[inline-c](https://github.com/fpco/inline-c/blob/master/README.md) Haskell
library.

The library is far from complete but the framework is there to easily bind
missing functionality.


Development
-----------

We use Nix to enter an environment containing all the needed dependencies.

    curl https://nixos.org/nix/install | sh   # Only execute this if you haven't installed Nix yet.
    nix-shell

Then you should be able to use `cabal` as normal.


Documentation
-------------

The project hasn't been released to Hackage yet so you won't find any
documentation there. Instead, we've uploaded the docs to
[our website](http://lumiguide.github.io/haskell-opencv/doc/index.html).


Examples
--------

The documenation includes example programs that are automatically extracted from
the source code and run as part of the test-suite. See the `opencv-doc-images`
test-suite. The resulting images are referenced from the Haddock documentation.
