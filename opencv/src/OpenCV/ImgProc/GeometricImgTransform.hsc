{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

{- |

The functions in this section perform various geometrical transformations of 2D
images. They do not change the image content but deform the pixel grid and map
this deformed grid to the destination image. In fact, to avoid sampling
artifacts, the mapping is done in the reverse order, from destination to the
source. That is, for each pixel @(x,y)@ of the destination image, the functions
compute coordinates of the corresponding "donor" pixel in the source image and
copy the pixel value:

@dst(x,y) = src(fx(x,y), fy(x,y))@

In case when you specify the forward mapping @\<gx,gy> : src -> dst@, the OpenCV
functions first compute the corresponding inverse mapping @\<fx,fy>:dst->src@
and then use the above formula.

The actual implementations of the geometrical transformations, from the most
generic remap and to the simplest and the fastest resize, need to solve two main
problems with the above formula:

* Extrapolation of non-existing pixels.
Similarly to the filtering functions described in the previous section, for some
@(x,y)@, either one of @fx(x,y)@, or @fy(x,y)@, or both of them may fall outside
of the image. In this case, an extrapolation method needs to be used. OpenCV
provides the same selection of extrapolation methods as in the filtering
functions. In addition, it provides the method 'BorderTransparent'. This means
that the corresponding pixels in the destination image will not be modified at
all.

* Interpolation of pixel values.
Usually @fx(x,y)@ and @fy(x,y)@ are floating-point numbers. This means that
@\<fx,fy>@ can be either an affine or perspective transformation, or radial lens
distortion correction, and so on. So, a pixel value at fractional coordinates
needs to be retrieved. In the simplest case, the coordinates can be just rounded
to the nearest integer coordinates and the corresponding pixel can be used. This
is called a nearest-neighbor interpolation. However, a better result can be
achieved by using more sophisticated interpolation methods , where a polynomial
function is fit into some neighborhood of the computed pixel
@(fx(x,y),fy(x,y))@, and then the value of the polynomial at @(fx(x,y),fy(x,y))@
is taken as the interpolated pixel value. In OpenCV, you can choose between
several interpolation methods. See resize for details.
-}
module OpenCV.ImgProc.GeometricImgTransform
    ( ResizeAbsRel(..)
    , resize
    , warpAffine
    , warpPerspective
    , invertAffineTransform
    , linearPolar
    , logPolar
    , getPerspectiveTransform
    , getAffineTransform
    , getRotationMatrix2D
    , remap
    , undistort
    ) where

import "base" Data.Int ( Int32 )
import "base" Data.Foldable
import "base" Foreign.C.Types ( CFloat, CDouble )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified Data.Vector as V
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import "linear" Linear.Vector ( zero )
import "mtl" Control.Monad.Error.Class ( MonadError )
import "this" OpenCV.Core.Types
import "this" OpenCV.ImgProc.Types
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Exception
import "this" OpenCV.Internal.ImgProc.Types
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
#ifdef OPENCV4
C.include "opencv2/calib3d.hpp"
#endif
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"

#include "namespace.hpp"

--------------------------------------------------------------------------------

data ResizeAbsRel
   = ResizeAbs Size2i -- ^ Resize to an absolute size.
   | ResizeRel (V2 Double)
     -- ^ Resize with relative factors for both the width and the height.
     deriving Show

marshalResizeAbsRel
    :: ResizeAbsRel
    -> (Size2i, CDouble, CDouble)
marshalResizeAbsRel (ResizeAbs s) = (s, 0   , 0   )
marshalResizeAbsRel (ResizeRel f) = (s, c'fx, c'fy)
  where
    s :: Size2i
    s = toSize (zero :: V2 Int32)

    (V2 c'fx c'fy) = realToFrac <$> f

{- | Resizes an image

To shrink an image, it will generally look best with 'InterArea' interpolation,
whereas to enlarge an image, it will generally look best with 'InterCubic'
(slow) or 'InterLinear' (faster but still looks OK).

Example:

@
resizeInterAreaImg :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
resizeInterAreaImg = exceptError $
    withMatM (h ::: w + (w \`div` 2) ::: Z)
             (Proxy :: Proxy 3)
             (Proxy :: Proxy Word8)
             transparent $ \\imgM -> do
      birds_resized <-
        resize (ResizeRel $ pure 0.5) InterArea birds_768x512
      matCopyToM imgM (V2 0 0) birds_768x512 Nothing
      matCopyToM imgM (V2 w 0) birds_resized Nothing
      lift $ arrowedLine imgM (V2 startX y) (V2 pointX y) red 4 LineType_8 0 0.15
  where
    [h, w] = miShape $ matInfo birds_768x512
    startX = round $ fromIntegral w * (0.95 :: Double)
    pointX = round $ fromIntegral w * (1.05 :: Double)
    y = h \`div` 4
@

<<doc/generated/examples/resizeInterAreaImg.png resizeInterAreaImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#resize OpenCV Sphinx doc>
-}
resize
    :: (MonadError CvException m)
    => ResizeAbsRel
    -> InterpolationMethod
    -> Mat ('S [height, width]) channels depth
    -> m (Mat ('S ['D, 'D]) channels depth)
resize factor interpolationMethod src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src   $ \srcPtr   ->
      withPtr dst   $ \dstPtr   ->
      withPtr dsize $ \dsizePtr ->
        [cvExcept|
          cv::resize
          ( *$(Mat * srcPtr)
          , *$(Mat * dstPtr)
          , *$(Size2i * dsizePtr)
          , $(double fx)
          , $(double fy)
          , $(int32_t c'interpolation)
          );
        |]
  where
    (dsize, fx, fy) = marshalResizeAbsRel factor
    c'interpolation = marshalInterpolationMethod interpolationMethod

#num WARP_FILL_OUTLIERS
#num WARP_INVERSE_MAP

{- | Applies an affine transformation to an image

Example:

@
rotateBirds :: Mat (ShapeT [2, 3]) ('S 1) ('S Double)
rotateBirds = getRotationMatrix2D (V2 256 170 :: V2 CFloat) 45 0.75

warpAffineImg :: Kodak_512x341
warpAffineImg = exceptError $
    warpAffine birds_512x341 rotateBirds InterArea False False (BorderConstant black)

warpAffineInvImg :: Kodak_512x341
warpAffineInvImg = exceptError $
    warpAffine warpAffineImg rotateBirds InterCubic True False (BorderConstant black)
@

<<doc/generated/birds_512x341.png             original        >>
<<doc/generated/examples/warpAffineImg.png    warpAffineImg   >>
<<doc/generated/examples/warpAffineInvImg.png warpAffineInvImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#warpaffine OpenCV Sphinx doc>
-}
warpAffine
    :: (MonadError CvException m, IsSize destSize Int32)
    => Mat ('S [height, width]) channels depth -- ^ Source image.
    -> Mat (ShapeT [2, 3]) ('S 1) ('S Double) -- ^ Affine transformation matrix.
    -> destSize Int32 -- ^ Destination size
    -> InterpolationMethod
    -> Bool -- ^ Perform the inverse transformation.
    -> Bool -- ^ Fill outliers.
    -> BorderMode -- ^ Pixel extrapolation method.
    -> m (Mat ('S ['D, 'D]) channels depth) -- ^ Transformed source image.
warpAffine src transform dshape interpolationMethod inverse fillOutliers borderMode =
    unsafeWrapException $ do
      dst <- newEmptyMat
      handleCvException (pure $ unsafeCoerceMat dst) $
        withPtr src $ \srcPtr ->
        withPtr dst $ \dstPtr ->
        withPtr transform   $ \transformPtr ->
        withPtr    borderValue $ \borderValuePtr ->
        withPtr dsize $ \dsizePtr ->
          [cvExcept|
            Mat * src = $(Mat * srcPtr);
            cv::warpAffine
              ( *src
              , *$(Mat * dstPtr)
              , *$(Mat * transformPtr)
              , *$(Size2i * dsizePtr)
              , $(int32_t c'interpolationMethod) | $(int32_t c'inverse) | $(int32_t c'fillOutliers)
              , $(int32_t c'borderMode)
              , *$(Scalar * borderValuePtr)
              );
          |]
  where
    c'interpolationMethod = marshalInterpolationMethod interpolationMethod
    c'inverse      = if inverse      then c'WARP_INVERSE_MAP   else 0
    c'fillOutliers = if fillOutliers then c'WARP_FILL_OUTLIERS else 0
    (c'borderMode, borderValue) = marshalBorderMode borderMode
    dsize = toSize dshape

-- | Applies a perspective transformation to an image
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#warpperspective OpenCV Sphinx doc>
warpPerspective
    :: (MonadError CvException m, IsSize destSize Int32)
    => Mat ('S [height, width]) channels depth -- ^ Source image.
    -> Mat (ShapeT [3, 3]) ('S 1) ('S Double) -- ^ Perspective transformation matrix.
    -> destSize Int32 -- ^ Destination size
    -> InterpolationMethod
    -> Bool -- ^ Perform the inverse transformation.
    -> Bool -- ^ Fill outliers.
    -> BorderMode -- ^ Pixel extrapolation method.
    -> m (Mat ('S ['D, 'D]) channels depth) -- ^ Transformed source image.
warpPerspective src transform dshape interpolationMethod inverse fillOutliers borderMode =
    unsafeWrapException $ do
      dst <- newEmptyMat
      handleCvException (pure $ unsafeCoerceMat dst) $
        withPtr src $ \srcPtr ->
        withPtr dst $ \dstPtr ->
        withPtr transform   $ \transformPtr   ->
        withPtr borderValue $ \borderValuePtr ->
        withPtr dsize $ \dsizePtr ->
          [cvExcept|
            Mat * src = $(Mat * srcPtr);
            cv::warpPerspective
              ( *src
              , *$(Mat * dstPtr)
              , *$(Mat * transformPtr)
              , *$(Size2i * dsizePtr)
              , $(int32_t c'interpolationMethod) | $(int32_t c'inverse) | $(int32_t c'fillOutliers)
              , $(int32_t c'borderMode)
              , *$(Scalar * borderValuePtr)
              );
          |]
  where
    c'interpolationMethod = marshalInterpolationMethod interpolationMethod
    c'inverse      = if inverse      then c'WARP_INVERSE_MAP   else 0
    c'fillOutliers = if fillOutliers then c'WARP_FILL_OUTLIERS else 0
    (c'borderMode, borderValue) = marshalBorderMode borderMode
    dsize = toSize dshape

-- | Inverts an affine transformation
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#invertaffinetransform OpenCV Sphinx doc>
invertAffineTransform
    :: (MonadError CvException m)
    => Mat (ShapeT [2, 3]) ('S 1) depth -- ^
    -> m (Mat (ShapeT [2, 3]) ('S 1) depth)
invertAffineTransform matIn = unsafeWrapException $ do
    matOut <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat matOut) $
      withPtr matIn  $ \matInPtr ->
      withPtr matOut $ \matOutPtr ->
        [cvExcept|
           cv::invertAffineTransform(*$(Mat * matInPtr), *$(Mat * matOutPtr));
        |]

{- | Remaps an image to polar coordinate space.

Transform the source image using the following transformation:

\[
\begin{array}{l}
  dst( \rho , \phi ) = src(x,y) \\
  dst.size() \leftarrow src.size()
\end{array}
\]

where

\[
\begin{array}{l}
  I = (dx,dy) = (x - center.x,y - center.y) \\
  \rho = Kx \cdot \texttt{magnitude} (I) ,\\
  \phi = Ky \cdot \texttt{angle} (I)_{0..360 deg}
\end{array}
\]

and

\[
\begin{array}{l}
  Kx = src.cols / maxRadius \\
  Ky = src.rows / 360
\end{array}
\]

Example:

@
linearPolarImg
    :: forall (width    :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *  )
     . (Mat ('S ['S height, 'S width]) ('S channels) ('S depth) ~ Compass)
    => Mat ('S ['S height, 'S width]) ('S channels) ('S depth)
linearPolarImg = exceptError $
    linearPolar compass (V2 250 248) 250 InterCubic False True
@

<<data/compass.jpg Untransformed image of a compass>>
<<doc/generated/examples/linearPolarImg.png linearPolarImg>>
-}
linearPolar
    :: (IsPoint2 point2 CFloat, MonadError CvException m)
    => Mat ('S [height, width]) channels depth -- ^ Source image.
    -> point2 CFloat -- ^ Center.
    -> Double -- ^ Max radius.
    -> InterpolationMethod -- ^ Interpolation algorithm.
    -> Bool -- ^ Inverse mapping.
    -> Bool -- ^ Fill outliers.
    -> m (Mat ('S [height, width]) channels depth)
linearPolar src center maxRadius interpolationMethod inverse fillOutliers =
    unsafeWrapException $ do
      dst <- newEmptyMat
      handleCvException (pure $ unsafeCoerceMat dst) $
        withPtr src $ \srcPtr ->
        withPtr dst $ \dstPtr ->
        withPtr (toPoint center) $ \centerPtr ->
          [cvExcept|
            cv::linearPolar
              ( *$(Mat * srcPtr)
              , *$(Mat * dstPtr)
              , *$(Point2f * centerPtr)
              , $(double c'maxRadius)
              , $(int32_t c'interpolationMethod) | $(int32_t c'inverse) | $(int32_t c'fillOutliers)
              );
          |]
  where
    c'maxRadius = toCDouble maxRadius
    c'interpolationMethod = marshalInterpolationMethod interpolationMethod
    c'inverse      = if inverse      then c'WARP_INVERSE_MAP   else 0
    c'fillOutliers = if fillOutliers then c'WARP_FILL_OUTLIERS else 0

{- | Remaps an image to semilog-polar coordinates space.

Transform the source image using the following transformation:

\[
\begin{array}{l}
  dst( \rho , \phi ) = src(x,y) \\
  dst.size() \leftarrow src.size()
\end{array}
\]

where

\[
\begin{array}{l}
  I = (dx,dy) = (x - center.x,y - center.y) \\
  \rho = M \cdot log_e(\texttt{magnitude} (I)) ,\\
  \phi = Ky \cdot \texttt{angle} (I)_{0..360 deg} \\
\end{array}
\]

and

\[
\begin{array}{l}
  M = src.cols / log_e(maxRadius) \\
  Ky = src.rows / 360 \\
\end{array}
\]

Example:

@
logPolarImg
    :: forall (width    :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *  )
     . (Mat ('S ['S height, 'S width]) ('S channels) ('S depth) ~ Compass)
    => Mat ('S ['S height, 'S width]) ('S channels) ('S depth)
logPolarImg = exceptError $
    logPolar compass (V2 250 248) (500 / log 250) InterCubic False True
@

<<data/compass.jpg Untransformed image of a compass>>
<<doc/generated/examples/logPolarImg.png logPolarImg>>
-}
logPolar
    :: (IsPoint2 point2 CFloat, MonadError CvException m)
    => Mat ('S [height, width]) channels depth -- ^ Source image.
    -> point2 CFloat -- ^ Center.
    -> Double -- ^ Magnitude scale.
    -> InterpolationMethod -- ^ Interpolation algorithm.
    -> Bool -- ^ Inverse mapping.
    -> Bool -- ^ Fill outliers.
    -> m (Mat ('S [height, width]) channels depth)
logPolar src center magnitudeScale interpolationMethod inverse fillOutliers =
    unsafeWrapException $ do
      dst <- newEmptyMat
      handleCvException (pure $ unsafeCoerceMat dst) $
        withPtr src $ \srcPtr ->
        withPtr dst $ \dstPtr ->
        withPtr (toPoint center) $ \centerPtr ->
          [cvExcept|
            cv::logPolar
              ( *$(Mat * srcPtr)
              , *$(Mat * dstPtr)
              , *$(Point2f * centerPtr)
              , $(double c'magnitudeScale)
              , $(int32_t c'interpolationMethod) | $(int32_t c'inverse) | $(int32_t c'fillOutliers)
              );
          |]
  where
    c'magnitudeScale = toCDouble magnitudeScale
    c'interpolationMethod = marshalInterpolationMethod interpolationMethod
    c'inverse      = if inverse      then c'WARP_INVERSE_MAP   else 0
    c'fillOutliers = if fillOutliers then c'WARP_FILL_OUTLIERS else 0


{- | Calculates a perspective transformation matrix for 2D perspective transform

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#getperspectivetransform OpenCV Sphinx doc>
-}
getPerspectiveTransform
    :: forall m point2. (IsPoint2 point2 CFloat, MonadError CvException m)
    => V4 (point2 CFloat) -- ^ Points representing 4 vertices in source image
    -> V4 (point2 CFloat) -- ^ Points representing 4 vertices in destination image
    -> m (Mat (ShapeT [3,3]) ('S 1) ('S Double)) -- ^ The output perspective transformation, 3x3 floating-point-matrix.
getPerspectiveTransform srcPts dstPts = pure $
  unsafeCoerceMat $
  unsafePerformIO $
  unsafeWithArrayPtr (v4ToVector srcPts) $ \srcPtsPtr ->
  unsafeWithArrayPtr (v4ToVector dstPts) $ \dstPtsPtr ->
  fromPtr
  [CU.block| Mat * {
      return new cv::Mat
        ( cv::getPerspectiveTransform($(Point2f * srcPtsPtr), $(Point2f * dstPtsPtr))
        );
  }|]
  where
    v4ToVector :: V4 (point2 CFloat) -> V.Vector Point2f
    v4ToVector = V.map toPoint . V.fromList . toList

{- | Calculates an affine transformation matrix for 2D affine transform

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#getaffinetransform OpenCV Sphinx doc>
-}
getAffineTransform
    :: forall m point2. (IsPoint2 point2 CFloat, MonadError CvException m)
    => V3 (point2 CFloat) -- ^ Points representing vertices in source image
    -> V3 (point2 CFloat) -- ^ Points representing vertices in destination image
    -> m (Mat (ShapeT [2, 3]) ('S 1) ('S Double))
       -- ^ The output affine transformation, 2x3 floating-point-matrix.
getAffineTransform srcPts dstPts = unsafeWrapException $ do
    result <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat result) $
      withPtr result $ \resultPtr ->
      unsafeWithArrayPtr (v3ToVector srcPts) $ \srcPtsPtr ->
      unsafeWithArrayPtr (v3ToVector dstPts) $ \dstPtsPtr ->
      [cvExcept|
        *$(Mat * resultPtr) =
          cv::getAffineTransform
            ( $(Point2f * srcPtsPtr)
            , $(Point2f * dstPtsPtr)
            );
      |]
  where
    v3ToVector :: V3 (point2 CFloat) -> V.Vector Point2f
    v3ToVector = V.map toPoint . V.fromList . toList

{- | Calculates an affine matrix of 2D rotation

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#getrotationmatrix2d OpenCV Sphinx doc>
-}
getRotationMatrix2D
    :: (IsPoint2 point2 CFloat)
    => point2 CFloat -- ^ Center of the rotation in the source image.
    -> Double
       -- ^ Rotation angle in degrees. Positive values mean counter-clockwise
       -- rotation (the coordinate origin is assumed to be the top-left corner).
    -> Double -- ^ Isotropic scale factor.
    -> Mat (ShapeT [2, 3]) ('S 1) ('S Double)
       -- ^ The output affine transformation, 2x3 floating-point matrix.
getRotationMatrix2D center angle scale = unsafeCoerceMat $ unsafePerformIO $
    withPtr (toPoint center) $ \centerPtr ->
      fromPtr
      [CU.block| Mat * {
        return new cv::Mat
        ( cv::getRotationMatrix2D
          ( *$(Point2f * centerPtr)
          , $(double c'angle)
          , $(double c'scale)
          )
        );
      }|]
  where
    c'angle = realToFrac angle
    c'scale = realToFrac scale

{- | Applies a generic geometrical transformation to an image.

The function remap transforms the source image using the specified map:

@dst(x,y) = src(map(x,y))@

Example:

@
remapImg
  :: forall (width    :: Nat)
            (height   :: Nat)
            (channels :: Nat)
            (depth    :: *  )
   . (Mat ('S ['S height, 'S width]) ('S channels) ('S depth) ~ Kodak_512x341)
  => Mat ('S ['S height, 'S width]) ('S channels) ('S depth)
remapImg = exceptError $ remap birds_512x341 transform InterLinear (BorderConstant black)
  where
    transform = exceptError $
                matFromFunc (Proxy :: Proxy [height, width])
                            (Proxy :: Proxy 2)
                            (Proxy :: Proxy Float)
                            exampleFunc

    exampleFunc [_y,  x] 0 = wobble x w
    exampleFunc [ y, _x] 1 = wobble y h
    exampleFunc _pos _channel = error "impossible"

    wobble :: Int -> Float -> Float
    wobble v s = let v' = fromIntegral v
                     n = v' / s
                 in v' + (s * 0.05 * sin (n * 2 * pi * 5))

    w = fromInteger $ natVal (Proxy :: Proxy width)
    h = fromInteger $ natVal (Proxy :: Proxy height)
@

<<doc/generated/birds_512x341.png original>>
<<doc/generated/examples/remapImg.png remapImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#remap OpenCV documentation>
-}
remap
    :: (MonadError CvException m)
    => Mat ('S [inputHeight, inputWidth]) inputChannels inputDepth
       -- ^ Source image.
    -> Mat ('S [outputHeight, outputWidth]) ('S 2) ('S Float)
       -- ^ A map of @(x, y)@ points.
    -> InterpolationMethod
       -- ^ Interpolation method to use. Note that 'InterArea' is not
       -- supported by this function.
    -> BorderMode
    -> m (Mat ('S [outputHeight, outputWidth]) inputChannels inputDepth)
remap src mapping interpolationMethod borderMode = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src $ \srcPtr ->
      withPtr dst $ \dstPtr ->
      withPtr mapping $ \mappingPtr ->
      withPtr borderValue $ \borderValuePtr ->
        [cvExcept|
          cv::Mat map2;
          cv::remap
            ( *$(Mat * srcPtr)
            , *$(Mat * dstPtr)
            , *$(Mat * mappingPtr)
            , map2
            , $(int32_t c'interpolation)
            , $(int32_t c'borderMode)
            , *$(Scalar * borderValuePtr)
            );
        |]
  where
    c'interpolation = marshalInterpolationMethod interpolationMethod
    (c'borderMode, borderValue) = marshalBorderMode borderMode


{-|

The function transforms an image to compensate radial and tangential lens
distortion.

Those pixels in the destination image, for which there is no correspondent
pixels in the source image, are filled with zeros (black color).

The camera matrix and the distortion parameters can be determined using
@calibrateCamera@ . If the resolution of images is different from the resolution
used at the calibration stage, f_x, f_y, c_x and c_y need to be scaled accordingly,
while the distortion coefficients remain the same.

Example:

@
undistortImg
  :: forall (width    :: Nat)
            (height   :: Nat)
            (channels :: Nat)
            (depth    :: *  )
   . (Mat ('S ['S height, 'S width]) ('S channels) ('S depth) ~ Kodak_512x341)
  => Mat ('S ['S height, 'S width]) ('S channels) ('S depth)
undistortImg = undistort birds_512x341 intrinsics coefficients
  where intrinsics :: M33 Float
        intrinsics =
          V3 (V3 15840.8      0      2049)
             (V3     0    15830.3    1097)
             (V3     0        0         1)

        coefficients :: Matx51d
        coefficients = unsafePerformIO $
          newMatx51d
            (-2.239145913492247)
             13.674526561736648
              3.650187848850095e-2
            (-2.0042015752853796e-2)
            (-0.44790921357620456)
@

<<doc/generated/birds_512x341.png original>>
<<doc/generated/examples/undistortImg.png undistortImg>>

-}
undistort
  :: ( ToMat m33d, MatShape m33d ~ 'S '[ 'S 3, 'S 3 ]
     , ToMat distCoeffs, MatShape distCoeffs `In` '[ 'S '[ 'S  4, 'S 1 ]
                                                   , 'S '[ 'S  5, 'S 1 ]
                                                   , 'S '[ 'S  8, 'S 1 ]
                                                   , 'S '[ 'S 12, 'S 1 ]
                                                   , 'S '[ 'S 14, 'S 1 ]
                                                   ]
     )
  => Mat ('S '[ h, w]) c d
    -- ^ The source image to undistort.
  -> m33d
  -- ^ The 3x3 matrix of intrinsic parameters.
  -> distCoeffs
  -- ^ The distortion coefficients
  --   (k1,k2,p1,p2[,k3[,k4,k5,k6[,s1,s2,s3,s4[,τx,τy]]]]) of 4, 5, 8, 12 or 14 elements.
  -> Mat ('S '[ h, w]) c d
undistort img camera distCoeffs = unsafePerformIO $ do
    dst <- newEmptyMat
    withPtr img $ \imgPtr ->
      withPtr dst $ \dstPtr ->
      withPtr (toMat camera) $ \cameraPtr ->
      withPtr (toMat distCoeffs) $ \distCoeffsPtr ->
        [C.block| void {
          undistort(*$(Mat * imgPtr),
                    *$(Mat * dstPtr),
                    *$(Mat * cameraPtr),
                    *$(Mat * distCoeffsPtr));
        }|]
    return (unsafeCoerceMat dst)
