#ifndef __HASKELL_OPENCV_SIMPLE_BLOB_DETECTOR_H__
#define __HASKELL_OPENCV_SIMPLE_BLOB_DETECTOR_H__

/*
This file defines some SimpleBlobDetector related names that are used in

  src/OpenCV/Features2d.hsc.

The reason we need these names is that we can't directly reference their
definitions because that would result in invalid syntax in either hsc2hs and
inline-c.
*/

typedef cv::Ptr<cv::SimpleBlobDetector> Ptr_SimpleBlobDetector;

#endif /* __HASKELL_OPENCV_SIMPLE_BLOB_DETECTOR_H__ */
