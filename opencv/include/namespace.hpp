#ifndef __OPENCV_NAMESPACE_H__
#define __OPENCV_NAMESPACE_H__

/*
Various .hsc files in this library need to refer to symbols in the cv namespace.
For example:

  #num EVENT_FLAG_LBUTTON

We can't just write:

  #num cv::EVENT_FLAG_LBUTTON

because that will result in invalid syntax. So instead we include this
header file which brings the cv namespace into scope.
*/

using namespace cv;

#endif /* __OPENCV_NAMESPACE_H__ */
