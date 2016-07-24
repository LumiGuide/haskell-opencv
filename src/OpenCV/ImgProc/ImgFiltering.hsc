{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#if __GLASGOW_HASKELL__ >= 800
{-# options_ghc -Wno-redundant-constraints #-}
#endif

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

    , laplacian
    , medianBlur
    , erode
    , dilate
    , morphologyEx
    , getStructuringElement
    , blur
    ) where

import "base" Data.Int
import "base" Data.Maybe
import "base" Data.Proxy
import "base" Data.Word
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.Types
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.Exception.Internal
import "this" OpenCV.ImgProc.Types
import "this" OpenCV.ImgProc.Types.Internal ( marshalBorderMode )
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"

#include "namespace.hpp"
#include "hsc_macros.hpp"


--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

defaultAnchor :: Point2i
defaultAnchor = toPoint2i (pure (-1) :: V2 Int32)


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data MorphShape
   = MorphRect -- ^ A rectangular structuring element.
   | MorphEllipse
     -- ^ An elliptic structuring element, that is, a filled ellipse inscribed
     -- into the rectangle Rect(0, 0, esize.width, 0.esize.height).
   | MorphCross !Point2i -- ^ A cross-shaped structuring element.

#num MORPH_RECT
#num MORPH_ELLIPSE
#num MORPH_CROSS

marshalMorphShape :: MorphShape -> (Int32, Point2i)
marshalMorphShape = \case
    MorphRect         -> (c'MORPH_RECT   , defaultAnchor)
    MorphEllipse      -> (c'MORPH_ELLIPSE, defaultAnchor)
    MorphCross anchor -> (c'MORPH_CROSS  , anchor)

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

{- | Calculates the Laplacian of an image

The function calculates the Laplacian of the source image by adding up
the second x and y derivatives calculated using the Sobel operator.

Example:

@
laplacianImg
    :: forall shape channels depth
     . (Mat shape channels depth ~ Birds_512x341)
    => Mat shape ('S 1) ('S Double)
laplacianImg = exceptError $ do
    imgG <- cvtColor bgr gray birds_512x341
    laplacian Nothing Nothing Nothing Nothing imgG
@

<<doc/generated/examples/laplacianImg.png laplacianImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/filtering.html#laplacian OpenCV Sphinx doc>
-}
laplacian
    :: forall shape channels srcDepth dstDepth
     . (ToDepth (Proxy dstDepth))
    => Maybe Int32
       -- ^ Aperture size used to compute the second-derivative filters. The
       -- size must be positive and odd. Default value is 1.
    -> Maybe Double
       -- ^ Optional scale factor for the computed Laplacian values. Default
       -- value is 1.
    -> Maybe Double
       -- ^ Optional delta value that is added to the results. Default value is
       -- 0.
    -> Maybe BorderMode
       -- ^ Pixel extrapolation method.
    -> Mat shape channels srcDepth
    -> CvExcept (Mat shape channels ('S dstDepth))
laplacian ksize scale delta borderType src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src $ \srcPtr ->
      withPtr dst $ \dstPtr ->
      [cvExcept|
        cv::Laplacian
        ( *$(Mat *   srcPtr      )
        , *$(Mat *   dstPtr      )
        ,  $(int32_t c'ddepth    )
        ,  $(int32_t c'ksize     )
        ,  $(double  c'scale     )
        ,  $(double  c'delta     )
        ,  $(int32_t c'borderType)
        );
      |]
  where
    c'ksize = fromMaybe 1 ksize
    c'scale = maybe 1 realToFrac scale
    c'delta = maybe 0 realToFrac delta
    c'ddepth = marshalDepth $ toDepth (Proxy :: Proxy dstDepth)
    c'borderType = fst $ marshalBorderMode $ fromMaybe BorderReflect101 borderType

{- | Blurs an image using the median filter

Example:

@
medianBlurImg
    :: forall (width    :: Nat)
              (width2   :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . ( Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Birds_512x341
       , width2 ~ ((*) width 2) -- TODO (RvD): HSE parse error with infix type operator
       )
    => Mat (ShapeT [height, width2]) ('S channels) ('S depth)
medianBlurImg = exceptError $
    withMatM (Proxy :: Proxy [height, width2])
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ \imgM -> do
      birdsBlurred <- pureExcept $ medianBlur birds_512x341 13
      matCopyToM imgM (V2 0 0) birds_512x341 Nothing
      matCopyToM imgM (V2 w 0) birdsBlurred  Nothing
  where
    w = fromInteger $ natVal (Proxy :: Proxy width)
@

<<doc/generated/examples/medianBlurImg.png medianBlurImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/filtering.html#medianblur OpenCV Sphinx doc>
-}
-- TODO (Rvd): make ksize a type level argument
-- if ksize in [3, 5] then depth in [Word8, Int16, Float) else depth ~ Word8
medianBlur
    :: ( depth    `In` '[Word8, Word16, Int16, Float, Double]
       , channels `In` '[1, 3, 4]
       -- , Length shape <= 2
       )
    => Mat shape ('S channels) ('S depth)
       -- ^ Input 1-, 3-, or 4-channel image; when ksize is 3 or 5, the image
       -- depth should be 'Word8', 'Word16', or 'Float', for
       -- larger aperture sizes, it can only be 'Word8'.
    -> Int32
       -- ^ Aperture linear size; it must be odd and greater than 1, for
       -- example: 3, 5, 7...
    -> CvExcept (Mat shape ('S channels) ('S depth))
medianBlur matIn ksize = unsafeWrapException $ do
    matOut <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat matOut) $
      withPtr matOut $ \matOutPtr ->
      withPtr matIn $ \matInPtr ->
        [cvExcept| cv::medianBlur(*$(Mat * matInPtr), *$(Mat * matOutPtr), $(int32_t ksize)); |]

{- | Blurs an image using a box filter.

Example:

@
boxBlurImg
    :: forall (width    :: Nat)
              (width2   :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . ( Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Birds_512x341
       , width2 ~ ((*) width 2) -- TODO (RvD): HSE parse error with infix type operator
       )
    => Mat (ShapeT [height, width2]) ('S channels) ('S depth)
boxBlurImg = exceptError $
    withMatM (Proxy :: Proxy [height, width2])
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ \imgM -> do
      birdsBlurred <- pureExcept $ blur (V2 13 13 :: V2 Int32) birds_512x341
      matCopyToM imgM (V2 0 0) birds_512x341 Nothing
      matCopyToM imgM (V2 w 0) birdsBlurred  Nothing
  where
    w = fromInteger $ natVal (Proxy :: Proxy width)
@

<<doc/generated/examples/boxBlurImg.png boxBlurImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/filtering.html#blur OpenCV Sphinx doc>
-}
blur
  :: (depth `In` '[Word8, Word16, Int16, Float, Double], ToSize2i size2i)
  => size2i -- ^ Blurring kernel size.
  -> Mat shape ('S channels) ('S depth)
  -> CvExcept (Mat shape ('S channels) ('S depth))
blur size matIn =
  unsafeWrapException $
  do matOut <- newEmptyMat
     handleCvException (pure $ unsafeCoerceMat matOut) $
       withPtr ksize $ \ksizePtr ->
       withPtr matIn $ \matInPtr ->
       withPtr matOut $ \matOutPtr ->
       [cvExcept|
           cv::blur
           ( *$(Mat * matInPtr)
           , *$(Mat * matOutPtr)
           , *$(Size2i * ksizePtr)
           );
       |]
  where ksize :: Size2i
        ksize = toSize2i size

{- | Erodes an image by using a specific structuring element

Example:

@
erodeImg
    :: forall (width    :: Nat)
              (width2   :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . ( Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Lambda
       , width2 ~ ((*) width 2) -- TODO (RvD): HSE parse error with infix type operator
       )
    => Mat (ShapeT [height, width2]) ('S channels) ('S depth)
erodeImg = exceptError $
    withMatM (Proxy :: Proxy [height, width2])
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ \imgM -> do
      erodedLambda <-
        pureExcept $ erode lambda Nothing (Nothing :: Maybe Point2i) 5 BorderReplicate
      matCopyToM imgM (V2 0 0) lambda Nothing
      matCopyToM imgM (V2 w 0) erodedLambda Nothing
  where
    w = fromInteger $ natVal (Proxy :: Proxy width)
@

<<doc/generated/examples/erodeImg.png erodeImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/filtering.html#erode OpenCV Sphinx doc>
-}
erode
    :: ( ToPoint2i point2i
       , depth `In` [Word8, Word16, Int16, Float, Double]
       )
    => Mat shape channels ('S depth) -- ^ Input image.
    -> Maybe (Mat ('S [sh, sw]) ('S 1) ('S Word8))
       -- ^ Structuring element used for erosion. If `emptyMat` is
       -- used a @3x3@ rectangular structuring element is used. Kernel
       -- can be created using `getStructuringElement`.
    -> Maybe point2i -- ^ anchor
    -> Int           -- ^ iterations
    -> BorderMode
    -> CvExcept (Mat shape channels ('S depth))
erode src mbKernel mbAnchor iterations borderMode = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src    $ \srcPtr    ->
      withPtr dst    $ \dstPtr    ->
      withPtr kernel $ \kernelPtr ->
      withPtr anchor $ \anchorPtr ->
      withPtr borderValue $ \borderValuePtr ->
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
    kernel :: Mat 'D 'D 'D
    kernel = maybe (relaxMat emptyMat) unsafeCoerceMat mbKernel

    anchor :: Point2i
    anchor = maybe defaultAnchor toPoint2i mbAnchor

    c'iterations = fromIntegral iterations
    (c'borderType, borderValue) = marshalBorderMode borderMode

{- | Dilates an image by using a specific structuring element

Example:

@
dilateImg
    :: forall (width    :: Nat)
              (width2   :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . ( Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Lambda
       , width2 ~ ((*) width 2) -- TODO (RvD): HSE parse error with infix type operator
       )
    => Mat (ShapeT [height, width2]) ('S channels) ('S depth)
dilateImg = exceptError $
    withMatM (Proxy :: Proxy [height, width2])
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ \imgM -> do
      dilatedLambda <-
        pureExcept $ dilate lambda Nothing (Nothing :: Maybe Point2i) 3 BorderReplicate
      matCopyToM imgM (V2 0 0) lambda Nothing
      matCopyToM imgM (V2 w 0) dilatedLambda Nothing
  where
    w = fromInteger $ natVal (Proxy :: Proxy width)
@

<<doc/generated/examples/dilateImg.png dilateImg>>


<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/filtering.html#dilate OpenCV Sphinx doc>
-}
dilate
    :: ( ToPoint2i point2i
       , depth `In` [Word8, Word16, Int16, Float, Double]
       )
    => Mat shape channels ('S depth) -- ^ Input image.
    -> Maybe (Mat ('S [sh, sw]) ('S 1) ('S Word8))
       -- ^ Structuring element used for dilation. If `emptyMat` is
       -- used a @3x3@ rectangular structuring element is used. Kernel
       -- can be created using `getStructuringElement`.
    -> Maybe point2i -- ^ anchor
    -> Int           -- ^ iterations
    -> BorderMode
    -> CvExcept (Mat shape channels ('S depth))
dilate src mbKernel mbAnchor iterations borderMode = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src    $ \srcPtr    ->
      withPtr dst    $ \dstPtr    ->
      withPtr kernel $ \kernelPtr ->
      withPtr anchor $ \anchorPtr ->
      withPtr borderValue $ \borderValuePtr ->
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
    kernel :: Mat 'D 'D 'D
    kernel = maybe (relaxMat emptyMat) unsafeCoerceMat mbKernel

    anchor :: Point2i
    anchor = maybe defaultAnchor toPoint2i mbAnchor

    c'iterations = fromIntegral iterations
    (c'borderType, borderValue) = marshalBorderMode borderMode

{- | Performs advanced morphological transformations

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/filtering.html#morphologyex OpenCV Sphinx doc>
-}
morphologyEx
    :: ( ToPoint2i point2i
       , depth `In` [Word8, Word16, Int16, Float, Double]
       )
     => Mat shape channels ('S depth) -- ^ Source image.
    -> MorphOperation  -- ^ Type of a morphological operation.
    -> Mat 'D 'D 'D    -- ^ Structuring element.
    -> Maybe point2i   -- ^ Anchor position with the kernel.
    -> Int             -- ^ Number of times erosion and dilation are applied.
    -> BorderMode
    -> CvExcept (Mat shape channels ('S depth))
morphologyEx src op kernel mbAnchor iterations borderMode = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src    $ \srcPtr    ->
      withPtr dst    $ \dstPtr    ->
      withPtr kernel $ \kernelPtr ->
      withPtr anchor $ \anchorPtr ->
      withPtr borderValue $ \borderValuePtr ->
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

    anchor :: Point2i
    anchor = maybe defaultAnchor toPoint2i mbAnchor

    c'iterations = fromIntegral iterations
    (c'borderType, borderValue) = marshalBorderMode borderMode


{- | Returns a structuring element of the specified size and shape for
morphological operations

Example:

@
type StructureImg = Mat (ShapeT [128, 128]) ('S 1) ('S Word8)

structureImg :: MorphShape -> StructureImg
structureImg shape = exceptError $ do
    mat <- getStructuringElement shape (Proxy :: Proxy 128) (Proxy :: Proxy 128)
    img <- matConvertTo (Just 255) Nothing mat
    bitwiseNot img

morphRectImg :: StructureImg
morphRectImg = structureImg MorphRect

morphEllipseImg :: StructureImg
morphEllipseImg = structureImg MorphEllipse

morphCrossImg :: StructureImg
morphCrossImg = structureImg $ MorphCross $ toPoint2i (pure (-1) :: V2 Int32)
@

<<doc/generated/examples/morphRectImg.png morphRectImg>>
<<doc/generated/examples/morphEllipseImg.png morphEllipseImg>>
<<doc/generated/examples/morphCrossImg.png morphCrossImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/filtering.html#getstructuringelement OpenCV Sphinx doc>
-}
getStructuringElement
    :: (ToInt32 height, ToInt32 width)
    => MorphShape -- ^
    -> height
    -> width
    -> CvExcept (Mat (ShapeT (height ::: width ::: Z)) ('S 1) ('S Word8))
getStructuringElement morphShape height width = unsafeWrapException $ do
    element <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat element) $
      withPtr ksize   $ \ksizePtr   ->
      withPtr anchor  $ \anchorPtr  ->
      withPtr element $ \elementPtr ->
       [cvExcept|
         *$(Mat * elementPtr) =
           cv::getStructuringElement
           ( $(int32_t c'morphShape)
           , *$(Size2i * ksizePtr)
           , *$(Point2i * anchorPtr)
           );
       |]
  where
    ksize :: Size2i
    ksize = toSize2i $ V2 (toInt32 width) (toInt32 height)
    (c'morphShape, anchor) = marshalMorphShape morphShape
