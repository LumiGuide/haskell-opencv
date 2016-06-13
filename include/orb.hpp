#ifndef __THEA_ORB_H__
#define __THEA_ORB_H__

/*
This file defines some ORB related names that are used in

  src/OpenCV/Features2d.hsc.

The reason we need these names is that we can't directly reference their
definitions because that would result in invalid syntax in either hsc2hs and
inline-c.
*/

#define HARRIS_SCORE cv::ORB::HARRIS_SCORE
#define FAST_SCORE   cv::ORB::FAST_SCORE

typedef cv::Ptr<cv::ORB> Ptr_ORB;

#endif /* __THEA_ORB_H__ */
