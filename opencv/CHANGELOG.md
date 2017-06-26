## To be released

### Changed

- Specify the more accurate `pkgconfig-depends: opencv >= 3.0.0` so that Cabal
  doesn't use the wrong version..
- Specify the more accurate `setup-depends: Cabal >= 1.23` to prevent building
  with unsupported Cabals.


## [0.0.1.1] - 2017-06-23

### Changed

- Fix test-suite by including missing data files.


## [0.0.1.0] - 2017-06-20

### Added

- OpenCV.Calib3d: findHomography.
- OpenCV.Core.ArrayOps: hconcat, vconcat.
- include/hsc_macros.hpp: #alignof macro.

### Changed

- Fix build on OS X.
- Add source repo to cabal file.
- Reference opencv-extra and the examples from the Cabal package description.


## 0.0.0.0 - 2017-06-11

- Initial version


[0.0.1.1]: https://github.com/LumiGuide/haskell-opencv/compare/opencv-0.0.1.0...opencv-0.0.1.1
[0.0.1.0]: https://github.com/LumiGuide/haskell-opencv/compare/opencv-0.0.0.0...opencv-0.0.1.0
