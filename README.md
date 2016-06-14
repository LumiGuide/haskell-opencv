Haskell OpenCV-3.1 binding
==========================

<img src="https://raw.githubusercontent.com/LumiGuide/haskell-opencv/master/data/haskell-opencv-logo.png" width="200px" alt="Haskell OpenCV-3.1 logo" />


This is a Haskell library providing a binding to OpenCV-3.1. It binds directly
with the C++ API using the
[inline-c](https://github.com/fpco/inline-c/blob/master/README.md) Haskell
library.

The library is far from complete but the framework is there to easily bind
missing functionality.


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


Development
-----------

We use Nix to enter an environment containing all the needed dependencies. For
the moment the following commands only work on Linux. The Nix expression for
OpenCV-3.1 currently fails to build on OS X.

    curl https://nixos.org/nix/install | sh   # Only execute this if you haven't installed Nix yet.
    nix-shell

Then you should be able to use `cabal` as normal.


Contributing
------------

We love to get contributions! Please send us your pull requests. Do make sure
your PR comes with Haddock documentation and preferably with an example
program. If you include your example program in the following way:

    {- | ...

    Example:

    @
    myExampleImg :: Mat (ShapeT [200, 300]) ('S 4) ('S Word8)
    myExampleImg = ...
    @

    <<doc/generated/examples/myExampleImg.png myExampleImg>>
    -}

then run `cabal test`, now your Haddock documentation will include a nice
picture with the output of your program. Not only that, but your example program
will at the same time function as a test for the function you just documented!


Policies
--------

In haskell-opencv we use the most precise types for integer arguments. OpenCV uses int which is a 32-bit integer. So in Haskell we have to use Int32 instead of Int for the width and height.
