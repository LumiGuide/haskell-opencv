{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Calib3d
    ( FundamentalMatMethod(..)
    , WhichImage(..)
    -- , calibrateCamera
    , findFundamentalMat
    , computeCorrespondEpilines
    ) where

import "base" Data.Int
import "base" Data.Word
import "base" Foreign.C.Types
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.Types
import "this" OpenCV.Calib3d.Constants
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.Exception
import "this" OpenCV.TypeLevel
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/calib3d.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------
-- Types

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

data WhichImage = Image1 | Image2 deriving (Show, Eq)

marshalWhichImage :: WhichImage -> Int32
marshalWhichImage = \case
    Image1 -> 1
    Image2 -> 2

--------------------------------------------------------------------------------

-- {- |
-- <http://docs.opencv.org/3.0-last-rst/modules/calib3d/doc/camera_calibration_and_3d_reconstruction.html#calibratecamera OpenCV Sphinx doc>
-- -}
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

The minimum number of points required depends on the 'FundamentalMatMethod'.

 * 'FM_7Point': @N == 7@
 * 'FM_8Point': @N >= 8@
 * 'FM_Ransac': @N >= 15@
 * 'FM_Lmeds': @N >= 8@

With 7 points the 'FM_7Point' method is used, despite the given method.

With more than 7 points the 'FM_7Point' method will be replaced by the
'FM_8Point' method.

Between 7 and 15 points the 'FM_Ransac' method will be replaced by the
'FM_Lmeds' method.

With the 'FM_7Point' method and with 7 points the result can contain up to 3
matrices, resulting in either 3, 6 or 9 rows. This is why the number of
resulting rows in tagged as 'D'ynamic. For all other methods the result always
contains 3 rows.

<http://docs.opencv.org/3.0-last-rst/modules/calib3d/doc/camera_calibration_and_3d_reconstruction.html#findfundamentalmat OpenCV Sphinx doc>
-}
findFundamentalMat
    :: ( Convert point2d Point2d
       )
    => V.Vector point2d -- ^ Points from the first image.
    -> V.Vector point2d -- ^ Points from the second image.
    -> FundamentalMatMethod
    -> Either CvException
              ( Maybe ( Mat ('S '[ 'D, 'S 3 ]) ('S 1) ('S Double)
                      , Mat ('S '[ 'D, 'D   ]) ('S 1) ('S Word8 )
                      )
              )
findFundamentalMat pts1 pts2 method = unsafePerformIO $ do
    fm   <- newEmptyMat
    pointMask <- newEmptyMat
    handleCvException (pure $ checkResult (fm, pointMask)) $
      withPtr fm $ \fmPtr ->
      withPtr pointMask $ \pointMaskPtr ->
      withArrayPtr (V.map convert pts1 :: V.Vector Point2d) $ \pts1Ptr ->
      withArrayPtr (V.map convert pts2 :: V.Vector Point2d) $ \pts2Ptr ->
        [cvExcept|
          cv::_InputArray pts1 = cv::_InputArray($(Point2d * pts1Ptr), $(int32_t c'numPts1));
          cv::_InputArray pts2 = cv::_InputArray($(Point2d * pts2Ptr), $(int32_t c'numPts2));
          *$(Mat * fmPtr) =
            cv::findFundamentalMat
            ( pts1
            , pts2
            , $(int32_t c'method)
            , $(double c'p1)
            , $(double c'p2)
            , *$(Mat * pointMaskPtr)
            );
        |]
  where
    c'numPts1 = fromIntegral $ V.length pts1
    c'numPts2 = fromIntegral $ V.length pts2
    (c'method, c'p1, c'p2) = marshalFundamentalMatMethod method

    checkResult (fm, pointMask) =
        case coerceMat fm of
          Left _errors -> Nothing
          Right fm' -> Just (fm', unsafeCoerceMat pointMask)

{- | For points in an image of a stereo pair, computes the corresponding epilines in the other image

<http://docs.opencv.org/3.0-last-rst/modules/calib3d/doc/camera_calibration_and_3d_reconstruction.html#computecorrespondepilines OpenCV Sphinx doc>
-}
computeCorrespondEpilines
    :: (Convert point2d Point2d)
    => V.Vector point2d -- ^ Points.
    -> WhichImage -- ^ Image which contains the points.
    -> Mat (ShapeT [3, 3]) ('S 1) ('S Double) -- ^ Fundamental matrix.
    -> Either CvException
              (Mat ('S ['D, 'S 1]) ('S 3) ('S Double))
computeCorrespondEpilines points whichImage fm = unsafePerformIO $ do
    epilines <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat epilines) $
      withArrayPtr (V.map convert points :: V.Vector Point2d) $ \pointsPtr ->
      withPtr fm       $ \fmPtr       ->
      withPtr epilines $ \epilinesPtr -> do
        -- Destroy type information about the pointsPtr. We wan't to generate
        -- C++ code that works for any type of point. Specifically Point2f and
        -- Point2d.
        [cvExcept|
          cv::_InputArray points =
            cv::_InputArray( $(Point2d * pointsPtr)
                           , $(int32_t c'numPoints)
                           );
          cv::computeCorrespondEpilines
          ( points
          , $(int32_t c'whichImage)
          , *$(Mat * fmPtr)
          , *$(Mat * epilinesPtr)
          );
        |]
  where
    c'numPoints = fromIntegral $ V.length points
    c'whichImage = marshalWhichImage whichImage
