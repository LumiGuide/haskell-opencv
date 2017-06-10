{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#if __GLASGOW_HASKELL__ >= 800
{-# options_ghc -Wno-redundant-constraints #-}
#endif

module OpenCV.ImgProc.MiscImgTransform
    ( -- * Color conversion
      cvtColor
    , module OpenCV.ImgProc.MiscImgTransform.ColorCodes

      -- * Flood filling
    , floodFill
    , FloodFillOperationFlags(..)
    , defaultFloodFillOperationFlags

      -- * Thresholding
    , ThreshType(..)
    , ThreshValue(..)
    , threshold

      -- * Watershed
    , watershed

      -- * GrabCut
    , GrabCutOperationMode(..)
    , grabCut

      -- * In range
    , inRange
    ) where

import "base" Data.Bits
import "base" Data.Int
import "base" Data.Proxy ( Proxy(..) )
import "base" Data.Word
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Storable ( peek )
import "base" GHC.TypeLits
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V4 ( V4 )
import "this" OpenCV.Core.Types
import "this" OpenCV.ImgProc.MiscImgTransform.ColorCodes
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Exception
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.ImgProc.MiscImgTransform
import "this" OpenCV.Internal.ImgProc.MiscImgTransform.TypeLevel
import "this" OpenCV.Internal.ImgProc.MiscImgTransform.ColorCodes ( colorConversionCode )
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

-- ignore next Haddock code block, because of the hash sign in the link at the end of the comment.
{- | Converts an image from one color space to another

The function converts an input image from one color space to
another. In case of a transformation to-from RGB color space, the
order of the channels should be specified explicitly (RGB or
BGR). Note that the default color format in OpenCV is often
referred to as RGB but it is actually BGR (the bytes are
reversed). So the first byte in a standard (24-bit) color image
will be an 8-bit Blue component, the second byte will be Green, and
the third byte will be Red. The fourth, fifth, and sixth bytes
would then be the second pixel (Blue, then Green, then Red), and so
on.

The conventional ranges for R, G, and B channel values are:

  * 0 to 255 for 'Word8' images

  * 0 to 65535 for 'Word16' images

  * 0 to 1 for 'Float' images

In case of linear transformations, the range does not matter. But
in case of a non-linear transformation, an input RGB image should
be normalized to the proper value range to get the correct results,
for example, for RGB to L*u*v* transformation. For example, if you
have a 32-bit floating-point image directly converted from an 8-bit
image without any scaling, then it will have the 0..255 value range
instead of 0..1 assumed by the function. So, before calling
'cvtColor', you need first to scale the image down:

>  cvtColor (img * 1/255) 'ColorConvBGR2Luv'

If you use 'cvtColor' with 8-bit images, the conversion will have
some information lost. For many applications, this will not be
noticeable but it is recommended to use 32-bit images in
applications that need the full range of colors or that convert an
image before an operation and then convert back.

If conversion adds the alpha channel, its value will set to the
maximum of corresponding channel range: 255 for 'Word8', 65535 for
'Word16', 1 for 'Float'.

Example:

@
cvtColorImg
    :: forall (width    :: Nat)
              (width2   :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . ( Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Birds_512x341
       , width2 ~ (width + width)
       )
    => Mat (ShapeT [height, width2]) ('S channels) ('S depth)
cvtColorImg = exceptError $
    withMatM ((Proxy :: Proxy height) ::: (Proxy :: Proxy width2) ::: Z)
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ \\imgM -> do
      birds_gray <- pureExcept $   cvtColor gray bgr
                               =<< cvtColor bgr gray birds_512x341
      matCopyToM imgM (V2 0 0) birds_512x341 Nothing
      matCopyToM imgM (V2 w 0) birds_gray    Nothing
      lift $ arrowedLine imgM (V2 startX midY) (V2 pointX midY) red 4 LineType_8 0 0.15
  where
    h, w :: Int32
    h = fromInteger $ natVal (Proxy :: Proxy height)
    w = fromInteger $ natVal (Proxy :: Proxy width)

    startX, pointX :: Int32
    startX = round $ fromIntegral w * (0.95 :: Double)
    pointX = round $ fromIntegral w * (1.05 :: Double)
    midY = h \`div\` 2
@

<<doc/generated/examples/cvtColorImg.png cvtColorImg>>

<http://goo.gl/3rfrhu OpenCV Sphinx Doc>
-}

-- the link avove is minified because it includes a hash, which the CPP tries to parse and fails

-- TODO (RvD): Allow value level color codes
-- Allow statically unknown color codes: fromColor :: DS ColorCode
cvtColor :: forall (fromColor   :: ColorCode)
                   (toColor     :: ColorCode)
                   (shape       :: DS [DS Nat])
                   (srcChannels :: DS Nat)
                   (dstChannels :: DS Nat)
                   (srcDepth    :: DS *)
                   (dstDepth    :: DS *)
          . ( ColorConversion fromColor toColor
            , ColorCodeMatchesChannels fromColor srcChannels
            , dstChannels ~ 'S (ColorCodeChannels toColor)
            , srcDepth `In` ['D, 'S Word8, 'S Word16, 'S Float]
            , dstDepth ~ ColorCodeDepth fromColor toColor srcDepth
            )
         => Proxy fromColor -- ^ Convert from 'ColorCode'. Make sure the source image has this 'ColorCode'
         -> Proxy toColor   -- ^ Convert to 'ColorCode'.
         -> Mat shape srcChannels srcDepth -- ^ Source image
         -> CvExcept (Mat shape dstChannels dstDepth)
cvtColor fromColor toColor src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src $ \srcPtr ->
      withPtr dst $ \dstPtr ->
        [cvExcept|
          cv::cvtColor( *$(Mat * srcPtr)
                      , *$(Mat * dstPtr)
                      , $(int32_t c'code)
                      , 0
                      );
        |]
  where
    c'code = colorConversionCode fromColor toColor

{- | The function 'floodFill' fills a connected component starting from the seed point with the specified color.

The connectivity is determined by the color/brightness closeness of the neighbor pixels. See the OpenCV
documentation for details on the algorithm.

Example:

@
floodFillImg
    :: forall (width    :: Nat)
              (width2   :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . ( Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Sailboat_768x512
       , width2 ~ (width + width)
       )
    => Mat (ShapeT [height, width2]) ('S channels) ('S depth)
floodFillImg = exceptError $
    withMatM ((Proxy :: Proxy height) ::: (Proxy :: Proxy width2) ::: Z)
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ \\imgM -> do
      sailboatEvening_768x512 <- thaw sailboat_768x512
      mask <- mkMatM (Proxy :: Proxy [height + 2, width + 2])
                     (Proxy :: Proxy 1)
                     (Proxy :: Proxy Word8)
                     black
      circle mask (V2 450 120 :: V2 Int32) 45 white (-1) LineType_AA 0
      rect <- floodFill sailboatEvening_768x512 (Just mask) seedPoint eveningRed (Just tolerance) (Just tolerance) defaultFloodFillOperationFlags
      rectangle sailboatEvening_768x512 rect blue 2 LineType_8 0
      frozenSailboatEvening_768x512 <- freeze sailboatEvening_768x512
      matCopyToM imgM (V2 0 0) sailboat_768x512 Nothing
      matCopyToM imgM (V2 w 0) frozenSailboatEvening_768x512 Nothing
      lift $ arrowedLine imgM (V2 startX midY) (V2 pointX midY) red 4 LineType_8 0 0.15
  where
    h, w :: Int32
    h = fromInteger $ natVal (Proxy :: Proxy height)
    w = fromInteger $ natVal (Proxy :: Proxy width)

    startX, pointX :: Int32
    startX = round $ fromIntegral w * (0.95 :: Double)
    pointX = round $ fromIntegral w * (1.05 :: Double)

    midY = h \`div\` 2

    seedPoint :: V2 Int32
    seedPoint = V2 100 50

    eveningRed :: V4 Double
    eveningRed = V4 0 100 200 255

    tolerance :: V4 Double
    tolerance = pure 7
@

<<doc/generated/examples/floodFillImg.png floodFillImg>>

<http://goo.gl/9XIIne OpenCV Sphinx Doc>
-}
floodFill
    :: ( PrimMonad m
       , channels `In` '[ 'S 1, 'S 3 ]
       , depth `In` '[ 'D, 'S Word8, 'S Float, 'S Double ]
       , IsPoint2 point2 Int32
       , ToScalar color
       )
    => Mut (Mat shape channels depth) (PrimState m)
        -- ^ Input/output 1- or 3-channel, 8-bit, or floating-point image. It is modified by the function unless the FLOODFILL_MASK_ONLY flag is set.
    -> Maybe (Mut (Mat (WidthAndHeightPlusTwo shape) ('S 1) ('S Word8)) (PrimState m))
        -- ^ Operation mask that should be a single-channel 8-bit image, 2 pixels wider and 2 pixels taller than image. Since this is both an input and output parameter, you must take responsibility of initializing it. Flood-filling cannot go across non-zero pixels in the input mask. For example, an edge detector output can be used as a mask to stop filling at edges. On output, pixels in the mask corresponding to filled pixels in the image are set to 1 or to the a value specified in flags as described below. It is therefore possible to use the same mask in multiple calls to the function to make sure the filled areas do not overlap.
        -- Note: Since the mask is larger than the filled image, a pixel  (x, y) in image corresponds to the pixel  (x+1, y+1) in the mask.
    -> point2 Int32
        -- ^ Starting point.
    -> color
        -- ^ New value of the repainted domain pixels.
    -> Maybe color
        -- ^ Maximal lower brightness/color difference between the currently observed pixel and one of its neighbors belonging to the component, or a seed pixel being added to the component. Zero by default.
    -> Maybe color
        -- ^ Maximal upper brightness/color difference between the currently observed pixel and one of its neighbors belonging to the component, or a seed pixel being added to the component. Zero by default.
    -> FloodFillOperationFlags
    -> m Rect2i
floodFill img mbMask seedPoint color mLoDiff mUpDiff opFlags =
    unsafePrimToPrim $
    withPtr img $ \matPtr ->
    withPtr mbMask $ \maskPtr ->
    withPtr (toPoint seedPoint) $ \seedPointPtr ->
    withPtr (toScalar color) $ \colorPtr ->
    withPtr loDiff $ \loDiffPtr ->
    withPtr upDiff $ \upDiffPtr ->
    withPtr rect $ \rectPtr -> do
      [C.block|void {
        cv::Mat * maskPtr = $(Mat * maskPtr);
        cv::floodFill( *$(Mat * matPtr)
                     , maskPtr ? cv::_InputOutputArray(*maskPtr) : cv::_InputOutputArray(noArray())
                     , *$(Point2i * seedPointPtr)
                     , *$(Scalar * colorPtr)
                     , $(Rect2i * rectPtr)
                     , *$(Scalar * loDiffPtr)
                     , *$(Scalar * upDiffPtr)
                     , $(int32_t c'opFlags)
                     );
      }|]
      pure rect
  where
    rect :: Rect2i
    rect = toRect HRect{ hRectTopLeft = pure 0
                       , hRectSize    = pure 0
                       }
    c'opFlags = marshalFloodFillOperationFlags opFlags
    zeroScalar = toScalar (pure 0 :: V4 Double)
    loDiff = maybe zeroScalar toScalar mLoDiff
    upDiff = maybe zeroScalar toScalar mUpDiff

data FloodFillOperationFlags
   = FloodFillOperationFlags
   { floodFillConnectivity :: Word8
      -- ^ Connectivity value. The default value of 4 means that only the four nearest neighbor pixels (those that share
      -- an edge) are considered. A connectivity value of 8 means that the eight nearest neighbor pixels (those that share
      -- a corner) will be considered.
   , floodFillMaskFillColor :: Word8
      -- ^ Value between 1 and 255 with which to fill the mask (the default value is 1).
   , floodFillFixedRange :: Bool
      -- ^ If set, the difference between the current pixel and seed pixel is considered. Otherwise, the difference
      -- between neighbor pixels is considered (that is, the range is floating).
   , floodFillMaskOnly :: Bool
      -- ^ If set, the function does not change the image ( newVal is ignored), and only fills the mask with the
      -- value specified in bits 8-16 of flags as described above. This option only make sense in function variants
      -- that have the mask parameter.
   }

defaultFloodFillOperationFlags :: FloodFillOperationFlags
defaultFloodFillOperationFlags =
    FloodFillOperationFlags
    { floodFillConnectivity = 4
    , floodFillMaskFillColor = 1
    , floodFillFixedRange = False
    , floodFillMaskOnly = False
    }

marshalFloodFillOperationFlags :: FloodFillOperationFlags -> Int32
marshalFloodFillOperationFlags opFlags =
    let connectivityBits = fromIntegral (floodFillConnectivity opFlags)
        maskFillColorBits = fromIntegral (floodFillMaskFillColor opFlags) `shiftL` 8
        fixedRangeBits = if floodFillFixedRange opFlags then c'FLOODFILL_FIXED_RANGE else 0
        fillMaskOnlyBits = if floodFillMaskOnly opFlags then c'FLOODFILL_MASK_ONLY else 0
    in connectivityBits .|. maskFillColorBits .|. fixedRangeBits .|. fillMaskOnlyBits

-- TODO (RvD): Otsu and triangle are only implemented for 8 bit images.

{- | Applies a fixed-level threshold to each array element

The function applies fixed-level thresholding to a single-channel array. The
function is typically used to get a bi-level (binary) image out of a
grayscale image or for removing a noise, that is, filtering out pixels with
too small or too large values. There are several types of thresholding
supported by the function.

Example:

@
grayBirds :: Mat (ShapeT [341, 512]) ('S 1) ('S Word8)
grayBirds = exceptError $ cvtColor bgr gray birds_512x341

threshBinaryBirds :: Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
threshBinaryBirds =
    exceptError $ cvtColor gray bgr $ fst $ exceptError $
    threshold (ThreshVal_Abs 100) (Thresh_Binary 150) grayBirds

threshBinaryInvBirds :: Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
threshBinaryInvBirds =
    exceptError $ cvtColor gray bgr $ fst $ exceptError $
    threshold (ThreshVal_Abs 100) (Thresh_BinaryInv 150) grayBirds

threshTruncateBirds :: Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
threshTruncateBirds =
    exceptError $ cvtColor gray bgr $ fst $ exceptError $
    threshold (ThreshVal_Abs 100) Thresh_Truncate grayBirds

threshToZeroBirds :: Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
threshToZeroBirds =
    exceptError $ cvtColor gray bgr $ fst $ exceptError $
    threshold (ThreshVal_Abs 100) Thresh_ToZero grayBirds

threshToZeroInvBirds :: Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
threshToZeroInvBirds =
    exceptError $ cvtColor gray bgr $ fst $ exceptError $
    threshold (ThreshVal_Abs 100) Thresh_ToZeroInv grayBirds
@

<<doc/generated/examples/threshBinaryBirds.png threshBinaryBirds>>
<<doc/generated/examples/threshBinaryInvBirds.png threshBinaryInvBirds>>
<<doc/generated/examples/threshTruncateBirds.png threshTruncateBirds>>
<<doc/generated/examples/threshToZeroBirds.png threshToZeroBirds>>
<<doc/generated/examples/threshToZeroInvBirds.png threshToZeroInvBirds>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/miscellaneous_transformations.html#threshold OpenCV Sphinx doc>
-}
threshold
    :: (depth `In` [Word8, Float])
    => ThreshValue -- ^
    -> ThreshType
    -> (Mat shape ('S 1) ('S depth))
    -> CvExcept (Mat shape ('S 1) ('S depth), Double)
threshold threshVal threshType src = unsafeWrapException $ do
    dst <- newEmptyMat
    alloca $ \calcThreshPtr ->
      handleCvException ((unsafeCoerceMat dst, ) . realToFrac <$> peek calcThreshPtr) $
      withPtr src $ \srcPtr ->
      withPtr dst $ \dstPtr ->
        [cvExcept|
          *$(double * calcThreshPtr) =
            cv::threshold( *$(Mat * srcPtr)
                         , *$(Mat * dstPtr)
                         , $(double c'threshVal)
                         , $(double c'maxVal)
                         , $(int32_t c'type)
                         );
        |]
  where
    c'type = c'threshType .|. c'threshValMode
    (c'threshType, c'maxVal) = marshalThreshType threshType
    (c'threshValMode, c'threshVal) = marshalThreshValue threshVal


{- | Performs a marker-based image segmentation using the watershed algorithm.

The function implements one of the variants of watershed, non-parametric marker-based segmentation algorithm, described in [Meyer, F. Color Image Segmentation, ICIP92, 1992].

Before passing the image to the function, you have to roughly outline the desired regions in the image markers with positive (>0) indices. So, every region is represented as one or more connected components with the pixel values 1, 2, 3, and so on. Such markers can be retrieved from a binary mask using 'findContours' and 'drawContours'. The markers are “seeds” of the future image regions. All the other pixels in markers , whose relation to the outlined regions is not known and should be defined by the algorithm, should be set to 0’s. In the function output, each pixel in markers is set to a value of the “seed” components or to -1 at boundaries between the regions.

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/miscellaneous_transformations.html#watershed OpenCV Sphinx doc>
-}
watershed
  :: (PrimMonad m)
  => Mat ('S [h, w]) ('S 3) ('S Word8) -- ^ Input 8-bit 3-channel image
  -> Mut (Mat ('S [h, w]) ('S 1) ('S Int32)) (PrimState m) -- ^ Input/output 32-bit single-channel image (map) of markers
  -> CvExceptT m ()
watershed img markers =
    unsafePrimToPrim $
    withPtr img $ \imgPtr ->
    withPtr markers $ \markersPtr ->
      [C.exp|void {
        cv::watershed( *$(Mat * imgPtr)
                     , *$(Mat * markersPtr)
                     )
      }|]

{- | Runs the <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/miscellaneous_transformations.html#grabcut GrabCut> algorithm.

Example:

@
grabCutBird :: Birds_512x341
grabCutBird = exceptError $ do
    mask <- withMatM (Proxy :: Proxy [341, 512])
                     (Proxy :: Proxy 1)
                     (Proxy :: Proxy Word8)
                     black $ \\mask -> do
      fgTmp <- mkMatM (Proxy :: Proxy [1, 65]) (Proxy :: Proxy 1) (Proxy :: Proxy Double) black
      bgTmp <- mkMatM (Proxy :: Proxy [1, 65]) (Proxy :: Proxy 1) (Proxy :: Proxy Double) black
      grabCut birds_512x341 mask fgTmp bgTmp 5 (GrabCut_InitWithRect rect)
    mask' <- matScalarCompare mask 3 Cmp_Ge
    withMatM (Proxy :: Proxy [341, 512])
             (Proxy :: Proxy 3)
             (Proxy :: Proxy Word8)
             transparent $ \\imgM -> do
      matCopyToM imgM (V2 0 0) birds_512x341 (Just mask')
  where
    rect :: Rect Int32
    rect = toRect $ HRect { hRectTopLeft = V2 264 60, hRectSize = V2 248 281 }
@

<<doc/generated/examples/grabCutBird.png grabCutBird>>

-}
grabCut
    :: ( PrimMonad m
       , depth `In` '[ 'D, 'S Word8 ]
       )
    => Mat shape ('S 3) depth
        -- ^ Input 8-bit 3-channel image.
    -> Mut (Mat shape ('S 1) ('S Word8)) (PrimState m)
        -- ^ Input/output 8-bit single-channel mask. The mask is initialized by the function when mode is set to GC_INIT_WITH_RECT. Its elements may have one of following values:
        --
        --     * GC_BGD defines an obvious background pixels.
        --
        --     * GC_FGD defines an obvious foreground (object) pixel.
        --
        --     * GC_PR_BGD defines a possible background pixel.
        --
        --     * GC_PR_FGD defines a possible foreground pixel.
    -> Mut (Mat ('S ['S 1, 'S 65]) ('S 1) ('S Double)) (PrimState m)
        -- ^ Temporary array for the background model. Do not modify it while you are processing the same image.
    -> Mut (Mat ('S ['S 1, 'S 65]) ('S 1) ('S Double)) (PrimState m)
        -- ^ Temporary arrays for the foreground model. Do not modify it while you are processing the same image.
    -> Int32
        -- ^ Number of iterations the algorithm should make before returning the result. Note that the result can be refined with further calls with mode==GC_INIT_WITH_MASK or mode==GC_EVAL.
    -> GrabCutOperationMode
        -- ^ Operation mode
    -> CvExceptT m ()
grabCut img mask bgdModel fgdModel iterCount mode =
    unsafePrimToPrim $
    withPtr img $ \imgPtr ->
    withPtr mask $ \maskPtr ->
    withPtr rect $ \rectPtr ->
    withPtr bgdModel $ \bgdModelPtr ->
    withPtr fgdModel $ \fgdModelPtr ->
      [C.block|void {
        cv::grabCut( *$(Mat * imgPtr)
                   , *$(Mat * maskPtr)
                   , *$(Rect2i * rectPtr)
                   , *$(Mat * bgdModelPtr)
                   , *$(Mat * fgdModelPtr)
                   , $(int32_t iterCount)
                   , $(int32_t c'modeFlags)
                   );
      }|]
  where
    rect = marshalGrabCutOperationModeRect mode
    c'modeFlags = marshalGrabCutOperationMode mode

{- | Returns 0 if the pixels are not in the range, 255 otherwise. -}
inRange ::
     (ToScalar scalar)
  => Mat ('S [w, h]) channels depth
  -> scalar -- ^ Lower bound
  -> scalar -- ^ Upper bound
  -> CvExcept (Mat ('S [w, h]) ('S 1) ('S Word8))
inRange src lo hi = unsafeWrapException $ do
  dst <- newEmptyMat
  withPtr src $ \srcPtr ->
    handleCvException (return (unsafeCoerceMat dst)) $
    withPtr (toScalar lo) $ \loPtr ->
    withPtr (toScalar hi) $ \hiPtr ->
    withPtr dst $ \dstPtr ->
      [cvExcept|
        cv::inRange(*$(Mat * srcPtr), *$(Scalar * loPtr), *$(Scalar * hiPtr), *$(Mat * dstPtr));
      |]
