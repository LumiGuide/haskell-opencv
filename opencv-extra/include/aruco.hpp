#ifndef __HASKELL_OPENCV_ARUCO_H__
#define __HASKELL_OPENCV_ARUCO_H__

#include <opencv2/aruco.hpp>
#include <opencv2/aruco/charuco.hpp>

namespace cv {
  typedef Matx<float, 5, 1> Matx51f;
  typedef Matx<double, 5, 1> Matx51d;
}

typedef std::vector< std::vector<cv::Point2f> > VectorVectorPoint2f;
typedef std::vector< int > VectorInt;
typedef std::vector< cv::Mat > VectorMat;
typedef cv::Ptr< cv::aruco::Dictionary > Ptr_Dictionary;
typedef cv::Ptr< cv::aruco::CharucoBoard > Ptr_CharucoBoard;
typedef cv::Ptr< cv::aruco::Board > Board;

#endif /* __HASKELL_OPENCV_ARUCO_H__ */
