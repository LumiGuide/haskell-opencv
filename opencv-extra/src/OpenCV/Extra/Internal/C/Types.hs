{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Extra.Internal.C.Types where

--------------------------------------------------------------------------------

-- | Haskell representation of an OpenCV @cv::Ptr<cv::bgsegm::BackgroundSubtractorGMG>@ object
data C'Ptr_BackgroundSubtractorGMG
-- | Haskell representation of an OpenCV @cv::Ptr<cv::bgsegm::Ptr_BackgroundSubtractorMOG>@ object
data C'Ptr_BackgroundSubtractorMOG
-- | Haskell representation of an OpenCV @cv::Ptr<cv::xfeatures2d::SURF>@ object
data C'Ptr_SURF

-- | Haskell representation of an OpenCV @cv::Ptr<cv::xphoto::GrayworldWB>@ object
data C'Ptr_GrayworldWB
-- | Haskell representation of an OpenCV @cv::Ptr<cv::xphoto::LearningBasedWB>@ object
data C'Ptr_LearningBasedWB
-- | Haskell representation of an OpenCV @cv::Ptr<cv::xphoto::SimpleWB>@ object
data C'Ptr_SimpleWB

-- | Haskell representation of an OpenCV @cv::Ptr<cv::Tracker>@ object
data C'Ptr_Tracker
data C'Ptr_TrackerFeature
data C'Ptr_MultiTracker
data C'Ptr_MultiTrackerAlt

data C'TrackerFeatureSet
data C'Ptr_TrackerSamplerAlgorithm


data C'Ptr'Dictionary
data C'Ptr'CharucoBoard

data C'Vector'Int

data C'Vector'Vector'Point2f
