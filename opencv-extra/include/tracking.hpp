#ifndef __HASKELL_OPENCV_TRACKING_H__
#define __HASKELL_OPENCV_TRACKING_H__

#include <opencv2/video/detail/tracking.detail.hpp>
#include <opencv2/tracking/tracking_legacy.hpp>

typedef cv::Ptr<cv::Tracker> Ptr_Tracker;
typedef cv::Ptr<cv::detail::tracking::TrackerFeature> Ptr_TrackerFeature;
typedef cv::Ptr<cv::legacy::tracking::MultiTracker> Ptr_MultiTracker;
typedef cv::Ptr<cv::legacy::tracking::MultiTracker_Alt> Ptr_MultiTrackerAlt;

#endif /* __HASKELL_OPENCV_TRACKING_H__ */
