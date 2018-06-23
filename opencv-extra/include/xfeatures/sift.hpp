#ifndef __OPENCV_XFEATURES_SIFT_H__
#define __OPENCV_XFEATURES_SIFT_H__

#include "opencv2/xfeatures2d.hpp"

/*
This file defines some SIFT related names that are used in

  src/OpenCV/XFeatures2d.hsc.

The reason we need these names is that we can't directly reference their
definitions because that would result in invalid syntax in either hsc2hs and
inline-c.
*/

typedef cv::Ptr<cv::xfeatures2d::SIFT> Ptr_SIFT;

#endif /* __OPENCV_XFEATURES_SIFT_H__ */
