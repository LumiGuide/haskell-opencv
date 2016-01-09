{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |

Functions and classes described in this section are used to perform various
linear or non-linear filtering operations on 2D images (represented as
@'Mat'@\'s). It means that for each pixel location @(x,y)@ in the source image
(normally, rectangular), its neighborhood is considered and used to compute the
response. In case of a linear filter, it is a weighted sum of pixel values. In
case of morphological operations, it is the minimum or maximum values, and so
on. The computed response is stored in the destination image at the same
location @(x,y)@. It means that the output image will be of the same size as the
input image. Normally, the functions support multi-channel arrays, in which case
every channel is processed independently. Therefore, the output image will also
have the same number of channels as the input one.

Another common feature of the functions and classes described in this section is
that, unlike simple arithmetic functions, they need to extrapolate values of
some non-existing pixels. For example, if you want to smooth an image using a
Gaussian @3x3@ filter, then, when processing the left-most pixels in each
row, you need pixels to the left of them, that is, outside of the image. You can
let these pixels be the same as the left-most image pixels ("replicated border"
extrapolation method), or assume that all the non-existing pixels are zeros
("constant border" extrapolation method), and so on. OpenCV enables you to
specify the extrapolation method.
-}
module OpenCV.ImgProc.ImgFiltering
    ( MorphShape(..)
    , MorphOperation(..)

    , medianBlur
    , erode
    , dilate
    , morphologyEx
    , getStructuringElement
    ) where

import "base" Foreign.C.Types
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lens" Control.Lens ( (^.), from )
import "linear" Linear.V2 ( V2(..) )
import "lumi-hackage-extended" Lumi.Prelude
import "this" Language.C.Inline.OpenCV ( openCvCtx )
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal
import "this" OpenCV.ImgProc.Types ( BorderMode )
import "this" OpenCV.ImgProc.Types.Internal ( marshalBorderMode )

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"

#include "namespace.hpp"
#include "macros.hpp"


--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

defaultAnchor :: Point2i
defaultAnchor = pure (-1 :: Int) ^. from isoPoint2iV2


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data MorphShape
   = MorphRect -- ^ A rectangular structuring element.
   | MorphEllipse
     -- ^ An elliptic structuring element, that is, a filled ellipse inscribed
     -- into the rectangle Rect(0, 0, esize.width, 0.esize.height).
   | MorphCross !(V2 Int) -- ^ A cross-shaped structuring element.

#num MORPH_RECT
#num MORPH_ELLIPSE
#num MORPH_CROSS

marshalMorphShape :: MorphShape -> (Int32, Point2i)
marshalMorphShape = \case
    MorphRect         -> (c'MORPH_RECT   , defaultAnchor)
    MorphEllipse      -> (c'MORPH_ELLIPSE, defaultAnchor)
    MorphCross anchor -> (c'MORPH_CROSS  , anchor ^. from isoPoint2iV2)

data MorphOperation
   = MorphOpen     -- ^ An opening operation: dilate . erode
   | MorphClose    -- ^ A closing operation: erode . dilate
   | MorphGradient -- ^ A morphological gradient: dilate - erode
   | MorphTopHat   -- ^ "top hat": src - open
   | MorphBlackHat -- ^ "black hat": close - src

#num MORPH_OPEN
#num MORPH_CLOSE
#num MORPH_GRADIENT
#num MORPH_TOPHAT
#num MORPH_BLACKHAT

marshalMorphOperation :: MorphOperation -> Int32
marshalMorphOperation = \case
    MorphOpen     -> c'MORPH_OPEN
    MorphClose    -> c'MORPH_CLOSE
    MorphGradient -> c'MORPH_GRADIENT
    MorphTopHat   -> c'MORPH_TOPHAT
    MorphBlackHat -> c'MORPH_BLACKHAT


--------------------------------------------------------------------------------
-- Image Filtering
--------------------------------------------------------------------------------

-- | Blurs an image using the median filter
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/filtering.html#medianblur OpenCV Sphinx doc>
medianBlur
    :: Mat
       -- ^ Input 1-, 3-, or 4-channel image; when ksize is 3 or 5, the image
       -- depth should be 'MatDepth_8U', 'MatDepth_16U', or 'MatDepth_32F', for
       -- larger aperture sizes, it can only be 'MatDepth_8U'.
    -> Int
       -- ^ Aperture linear size; it must be odd and greater than 1, for
       -- example: 3, 5, 7...
    -> Either CvException Mat
medianBlur matIn ksize = unsafePerformIO $ c'medianBlur $ fromIntegral ksize
  where
    c'medianBlur :: CInt -> IO (Either CvException Mat)
    c'medianBlur c'ksize = do
      matOut <- newEmptyMat
      handleCvException (pure matOut) $
        withMatPtr matOut $ \matOutPtr ->
        withMatPtr matIn $ \matInPtr ->
          [cvExcept| cv::medianBlur(*$(Mat * matInPtr), *$(Mat * matOutPtr), $(int c'ksize)); |]

-- | Erodes an image by using a specific structuring element
erode
    :: Mat
    -> Mat
    -> Maybe Point2i
    -> Int
    -> BorderMode
    -> Either CvException Mat
erode src kernel mbAnchor iterations borderMode = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
      withMatPtr     src         $ \srcPtr         ->
      withMatPtr     dst         $ \dstPtr         ->
      withMatPtr     kernel      $ \kernelPtr      ->
      withPoint2iPtr anchor      $ \anchorPtr      ->
      withScalarPtr  borderValue $ \borderValuePtr ->
        [cvExcept|
          cv::erode
          ( *$(Mat     * srcPtr        )
          , *$(Mat     * dstPtr        )
          , *$(Mat     * kernelPtr     )
          , *$(Point2i * anchorPtr     )
          ,  $(int32_t   c'iterations  )
          ,  $(int32_t   c'borderType  )
          , *$(Scalar  * borderValuePtr)
          );
        |]
  where
    anchor = fromMaybe defaultAnchor mbAnchor
    c'iterations = fromIntegral iterations
    (c'borderType, borderValue) = marshalBorderMode borderMode

-- | Dilates an image by using a specific structuring element
dilate
    :: Mat
    -> Mat
    -> Maybe Point2i
    -> Int
    -> BorderMode
    -> Either CvException Mat
dilate src kernel mbAnchor iterations borderMode = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
      withMatPtr     src         $ \srcPtr         ->
      withMatPtr     dst         $ \dstPtr         ->
      withMatPtr     kernel      $ \kernelPtr      ->
      withPoint2iPtr anchor      $ \anchorPtr      ->
      withScalarPtr  borderValue $ \borderValuePtr ->
        [cvExcept|
          cv::dilate
          ( *$(Mat     * srcPtr        )
          , *$(Mat     * dstPtr        )
          , *$(Mat     * kernelPtr     )
          , *$(Point2i * anchorPtr     )
          ,  $(int32_t   c'iterations  )
          ,  $(int32_t   c'borderType  )
          , *$(Scalar  * borderValuePtr)
          );
        |]
  where
    anchor = fromMaybe defaultAnchor mbAnchor
    c'iterations = fromIntegral iterations
    (c'borderType, borderValue) = marshalBorderMode borderMode

morphologyEx
    :: Mat
       -- ^ Source image. The number of channels can be arbitrary. The depth
       -- should be one of 'MatDepth_8U', 'MatDepth_16U', 'MatDepth_16S',
       -- 'MatDepth_32F' or 'MatDepth_64F'.
    -> MorphOperation -- ^ Type of a morphological operation.
    -> Mat            -- ^ Structuring element.
    -> Maybe Point2i  -- ^ Anchor position with the kernel.
    -> Int            -- ^ Number of times erosion and dilation are applied.
    -> BorderMode
    -> Either CvException Mat
morphologyEx src op kernel mbAnchor iterations borderMode = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
      withMatPtr     src         $ \srcPtr         ->
      withMatPtr     dst         $ \dstPtr         ->
      withMatPtr     kernel      $ \kernelPtr      ->
      withPoint2iPtr anchor      $ \anchorPtr      ->
      withScalarPtr  borderValue $ \borderValuePtr ->
        [cvExcept|
          cv::morphologyEx
          ( *$(Mat     * srcPtr        )
          , *$(Mat     * dstPtr        )
          ,  $(int32_t   c'op          )
          , *$(Mat     * kernelPtr     )
          , *$(Point2i * anchorPtr     )
          ,  $(int32_t   c'iterations  )
          ,  $(int32_t   c'borderType  )
          , *$(Scalar  * borderValuePtr)
          );
        |]

  where
    c'op = marshalMorphOperation op
    anchor = fromMaybe defaultAnchor mbAnchor
    c'iterations = fromIntegral iterations
    (c'borderType, borderValue) = marshalBorderMode borderMode


-- | Returns a structuring element of the specified size and shape for
-- morphological operations
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/filtering.html#getstructuringelement OpenCV Sphinx doc>
getStructuringElement
    :: MorphShape
    -> Size2i
    -> Either CvException Mat
getStructuringElement morphShape ksize = unsafePerformIO $ do
    element <- newEmptyMat
    handleCvException (pure element) $
      withSize2iPtr  ksize   $ \ksizePtr   ->
      withPoint2iPtr anchor  $ \anchorPtr  ->
      withMatPtr     element $ \elementPtr ->
       [cvExcept|
         *$(Mat * elementPtr) =
           cv::getStructuringElement
           ( $(int32_t c'morphShape)
           , *$(Size2i * ksizePtr)
           , *$(Point2i * anchorPtr)
           );
       |]
  where
    (c'morphShape, anchor) = marshalMorphShape morphShape
