#ifndef __OPENCV_XFEATURES_SIFT_H__
#define __OPENCV_XFEATURES_SIFT_H__

// Note that `opencv2/xfeatures2d.hpp` does not exist in many OpenCV installations
// because it requires the `OPENCV_ENABLE_NONFREE` option (SIFT and SURF are patented).
#include "opencv2/xfeatures2d.hpp"

/*
This file defines some SIFT related names that are used in

  src/OpenCV/XFeatures2d.hs.

The reason we need these names is that we can't directly reference their
definitions because that would result in invalid syntax in either hsc2hs and
inline-c.
*/

typedef cv::Ptr<cv::SIFT> Ptr_SIFT;

#endif /* __OPENCV_XFEATURES_SIFT_H__ */
