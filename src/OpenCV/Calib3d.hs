{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Calib3d
    ( FundamentalMatMethod(..)
    -- , calibrateCamera
    , findFundamentalMat
    -- , CvtToHomogeneousDepthT
    -- , convertPointsToHomogeneous
    ) where

import "base" Data.Int
import "base" Data.Word
import "base" Foreign.C.Types
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Exception
import "this" OpenCV.Calib3d.Constants
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.TypeLevel
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/calib3d.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

data FundamentalMatMethod
   = FM_7Point
   | FM_8Point
   | FM_Ransac !(Maybe Double) !(Maybe Double)
   | FM_Lmeds  !(Maybe Double)
     deriving (Show, Eq)

marshalFundamentalMatMethod :: FundamentalMatMethod -> (Int32, CDouble, CDouble)
marshalFundamentalMatMethod = \case
    FM_7Point       -> (c'CV_FM_7POINT, 0, 0)
    FM_8Point       -> (c'CV_FM_8POINT, 0, 0)
    FM_Ransac p1 p2 -> (c'CV_FM_RANSAC, maybe 3 realToFrac p1, maybe 0.99 realToFrac p2)
    FM_Lmeds     p2 -> (c'CV_FM_LMEDS, 0, maybe 0.99 realToFrac p2)

--------------------------------------------------------------------------------

{- |
<http://docs.opencv.org/3.0-last-rst/modules/calib3d/doc/camera_calibration_and_3d_reconstruction.html#calibratecamera OpenCV Sphinx doc>
-}
-- calibrateCamera
--     :: ( ToSize2i imageSize
--        , camMat ~ Mat (ShapeT [3, 3]) ('S 1) ('S Double)
--        )
--      . V.Vector () -- combine objectPoints and imagePoints
--     -> imageSize
--     -> camMat
--     -> flags
--     -> criteria
--     -> (camMat, distCoeffs, rvecs, tvecs)
-- calibrateCamera = _todo

{- | Calculates a fundamental matrix from the corresponding points in two images

<http://docs.opencv.org/3.0-last-rst/modules/calib3d/doc/camera_calibration_and_3d_reconstruction.html#findfundamentalmat OpenCV Sphinx doc>
-}
findFundamentalMat
    :: ( ToPoint2d point2d
       )
    => V.Vector point2d -- ^ Points from the first image.
    -> V.Vector point2d -- ^ Points from the second image.
    -> FundamentalMatMethod
    -> Either CvException
              ( Mat ('S '[ 'D, 'S 3 ]) ('S 1) ('S Double)
              , Mat ('S '[ 'D]       ) ('S 1) ('S Word8 )
              )
findFundamentalMat points1 points2 method = unsafePerformIO $ do
    fm   <- newEmptyMat
    mask <- newEmptyMat
    handleCvException (pure (unsafeCoerceMat fm, unsafeCoerceMat mask)) $
      withMatPtr fm   $ \fmPtr   ->
      withMatPtr mask $ \maskPtr ->
      withPoint2ds points1 $ \pts1Ptr ->
      withPoint2ds points2 $ \pts2Ptr ->
        [cvExcept|
          cv::_InputArray pts1 = cv::_InputArray($(Point2d * pts1Ptr), $(int32_t c'numPoints1));
          cv::_InputArray pts2 = cv::_InputArray($(Point2d * pts2Ptr), $(int32_t c'numPoints2));
          *$(Mat * fmPtr) =
            cv::findFundamentalMat
            ( pts1
            , pts2
            , $(int32_t c'method)
            , $(double c'p1)
            , $(double c'p2)
            , *$(Mat * maskPtr)
            );
        |]
  where
    c'numPoints1 = fromIntegral $ V.length points1
    c'numPoints2 = fromIntegral $ V.length points2
    (c'method, c'p1, c'p2) = marshalFundamentalMatMethod method

-- type family CvtToHomogeneousDepthT (depthIn :: *) :: * where
--     CvtToHomogeneousDepthT Int32  = Float
--     CvtToHomogeneousDepthT Float  = Float
--     CvtToHomogeneousDepthT Double = Double

-- {- | Converts points from Euclidean to homogeneous space

-- <http://docs.opencv.org/3.0-last-rst/modules/calib3d/doc/camera_calibration_and_3d_reconstruction.html#convertpointstohomogeneous OpenCV Sphinx doc>
-- -}
-- convertPointsToHomogeneous
--     :: ( dimIn `In` [2, 3]
--        , dimOut ~ dimIn + 1
--        )
--     => Mat ('S '[numPts, 'S dimIn ]) ('S 1) depth
--     -> Mat ('S '[numPts, 'S dimOut]) ('S 1) (CvtToHomogeneousDepthT depth)
