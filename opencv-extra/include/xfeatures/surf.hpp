#ifndef __OPENCV_XFEATURES_SURF_H__
#define __OPENCV_XFEATURES_SURF_H__

#include "opencv2/xfeatures2d.hpp"

/*
This file defines some SURF related names that are used in

  src/OpenCV/XFeatures2d.hsc.

The reason we need these names is that we can't directly reference their
definitions because that would result in invalid syntax in either hsc2hs and
inline-c.
*/

// TODO #define HARRIS_SCORE cv::ORB::HARRIS_SCORE
// TODO #define FAST_SCORE   cv::ORB::FAST_SCORE

typedef cv::Ptr<cv::xfeatures2d::SURF> Ptr_SURF;

#endif /* __OPENCV_XFEATURES_SURF_H__ */
