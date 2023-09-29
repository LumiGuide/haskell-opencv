## To be released

### Changed

- OpenCV 4 requirement:
  - OpenCV 3 compatibility was dropped to ease maintenance.
    For example, Ubuntu 22.04 (1.5 years old as of writing) does not ship OpenCV 3.
    If you want this back, please contributed it:
    It will require a reasonable amount of `#if`s that can probably be added in 1 day, but would more importantly need a CI setup added so it keeps working.
  - Gained a Cabal flag `enable-nonfree` to enable those modules that require the system OpenCV 4 to be compiled with the `OPENCV_ENABLE_NONFREE` option (most OpenCV installations lack this by default).
    This means SIFT/SURF feature detection are not available by default.
    (Side note: The SIFT patent expired in 2020, thus OpenCV 4.4 supposedly moved SIFT to the main repo, see https://opencv.org/blog/2020/07/18/opencv-4-4-0/, but this Haskell package has not been updated accordingly yet.)
    Note that the authors of this Haskell package do not currently fully understand OpenCV's logic here:
    On Ubuntu 22.04, OpenCV 4.5 does not include the `opencv2/xfeatures2d/nonfree.hpp` header.
    But on NixOS 23.05, `opencv4` version 4.7 does include it, even though `-DOPENCV_ENABLE_NONFREE=OFF` was passed to its build, and so the code builds with the `enable-nonfree` Cabal flag in any case.
  - Legacy tracker types were removed.
    OpenCV has [split](https://docs.opencv.org/4.8.0/d9/df8/group__tracking.html) their API into "Tracking API" and "Legacy Tracking API.
    These have incompatible `Tracker` type hierarchies:
    [`cv::Tracker`](https://docs.opencv.org/4.8.0/d0/d0a/classcv_1_1Tracker.html) and
    [`cv::legacy::Tracker`](https://docs.opencv.org/4.8.0/db/dfe/classcv_1_1legacy_1_1Tracker.html).
    For simplicity, this Haskell binding dropped the legacy API's `TrackerType`s trackers:
    - `BOOSTING`
    - `MEDIANFLOW`
    - `TLD`
    If you need these, please let us know and ideally contribute a wrapper for them.
  - `initTracker` changed its return type from `Bool` to `()` as the C++ return type is now `void`
    - [3.4](https://docs.opencv.org/3.4/d0/d0a/classcv_1_1Tracker.html#a4d285747589b1bdd16d2e4f00c3255dc)
    - [4.8](https://docs.opencv.org/4.8.0/d0/d0a/classcv_1_1Tracker.html#a7793a7ccf44ad5c3557ea6029a42a198)
  - `initTracker` and `updateTracker` changed their initial `boundingBox` related argument types from `double` to `int`.
    - This is because the C++ types changed from `Rect2d` to `Rect` (which is `Rect2i`), for example:
      - [3.4](https://docs.opencv.org/3.4/d0/d0a/classcv_1_1Tracker.html#a549159bd0553e6a8de356f3866df1f18)
      - [4.8](https://docs.opencv.org/4.8.0/d0/d0a/classcv_1_1Tracker.html#a92d2012f576e6c06eb2e257d110a6529)
    - For example,  `IsRect rect C.CDouble` to `IsRect rect Int32`.
  - The `TrackerFeatureType` was extended to have the `FEATURE2D` constructor carry 2 `String` fields `detectorType` and `descriptorType`.
    The previous argument-less type did not really make sense, as one cannot construct a `Feature2d` tracker feature without specifying these.
    This was possible in the previous OpenCV 3 API only because its docs said `The modes available now: "HAAR"` and for all other types, including `Feature2d`, it said `The modes that will be available soon`, so that likely never worked.
    This change should allow to use the many more feature types from [`Feature2D`](https://docs.opencv.org/4.8.0/d0/d13/classcv_1_1Feature2D.html).
- Support versions of Glob < 0.9.


## [0.2.0.1] - 2018-01-01

### Changed

- opencv-extra now only builds on GHC >= 8.2.


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


[0.2.0.1]: https://github.com/LumiGuide/haskell-opencv/compare/opencv-extra-0.2.0.0...opencv-extra-0.2.0.1
[0.2.0.0]: https://github.com/LumiGuide/haskell-opencv/compare/opencv-extra-0.1.0.0...opencv-extra-0.2.0.0
[0.1.0.0]: https://github.com/LumiGuide/haskell-opencv/compare/opencv-extra-0.0.0.1...opencv-extra-0.1.0.0
[0.0.0.1]: https://github.com/LumiGuide/haskell-opencv/compare/opencv-extra-0.0.0.0...opencv-extra-0.0.0.1
