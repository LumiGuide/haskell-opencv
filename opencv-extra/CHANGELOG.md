## [0.2.0.0] - 2017-11-06

- Support OpenCV-3.3.x

### Changed

- Added the GOTURN constructor to the OpenCV.Extra.Tracking.TrackerType datatype.
- Specify the more accurate `pkgconfig-depends: opencv >= 3.0.0` so that Cabal
  doesn't use the wrong version..
- Specify the more accurate `setup-depends: Cabal >= 1.23` to prevent building
  with unsupported Cabals.


## [0.1.0.0] - 2017-06-23

### Added

- Added all constructors of `OpenCV.Extra.ArUco.PredefinedDictionaryName`.

### Changed

- Fix test-suite by including missing data files.


## [0.0.0.1] - 2017-06-20

### Changed

- Fix build on OS X.
- Add source repo to cabal file.
- Added a Cabal package description.


## 0.0.0.0 - 2017-06-11

- Initial version


[0.2.0.0]: https://github.com/LumiGuide/haskell-opencv/compare/opencv-extra-0.1.0.0...opencv-extra-0.2.0.0
[0.1.0.0]: https://github.com/LumiGuide/haskell-opencv/compare/opencv-extra-0.0.0.1...opencv-extra-0.1.0.0
[0.0.0.1]: https://github.com/LumiGuide/haskell-opencv/compare/opencv-extra-0.0.0.0...opencv-extra-0.0.0.1
