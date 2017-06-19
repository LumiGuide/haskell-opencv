[![Build Status](https://travis-ci.org/LumiGuide/haskell-opencv.svg)](https://travis-ci.org/LumiGuide/haskell-opencv)

[opencv ![Hackage](https://img.shields.io/hackage/v/opencv.svg)](https://hackage.haskell.org/package/opencv)

[opencv-extra ![Hackage](https://img.shields.io/hackage/v/opencv-extra.svg)](https://hackage.haskell.org/package/opencv-extra)

Haskell OpenCV-3.x binding
==========================

<img src="https://raw.githubusercontent.com/LumiGuide/haskell-opencv/master/data/haskell-opencv-logo-200x82.png" alt="Haskell OpenCV-3.x logo" />


This is a Haskell library providing a binding to OpenCV-3.x. It binds directly
with the C++ API using the
[inline-c](https://github.com/fpco/inline-c/blob/master/README.md) Haskell
library.

The library is far from complete but the framework is there to easily bind
missing functionality.

Test Coverage
-------------

[HPC Coverage Report](http://lumiguide.github.io/haskell-opencv/hpc/hpc_index.html)

Examples
--------

The documenation includes example programs that are automatically extracted from
the source code and run as part of the test-suite. See the `opencv-doc-images`
test-suite. The resulting images are referenced from the Haddock documentation.

Internal modules
----------------

OpenCV exports a number of modules named OpenCV.Internal. They are
provided in case of urgent need for access to the internals, but they
are not intended to be used by API consumers and if you find yourself
repeatedly accessing them this is a sign that either you or OpenCV are
doing something wrong. In such a case please file a bug.

The interface of Internal modules does not follow the PVP and may
break between minor releases, so be careful.

Development
-----------

To get into an environment that contains all the needed dependencies we use Nix. 
The following commands work both on Linux and OS X:

    curl https://nixos.org/nix/install | sh   # Only execute this if you haven't installed Nix yet.
    cd opencv[-extra]
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

In haskell-opencv we use the most precise types for integer
arguments. OpenCV uses int which is a 32-bit integer. So in Haskell we
have to use Int32 instead of Int for, eg. width and height.
