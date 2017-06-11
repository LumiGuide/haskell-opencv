{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#if __GLASGOW_HASKELL__ >= 800
{-# options_ghc -Wno-redundant-constraints #-}
#endif

module OpenCV.Photo
  ( InpaintingMethod(..)
  , decolor
  , inpaint
  , denoise_TVL1
  , fastNlMeansDenoisingColored
  , fastNlMeansDenoisingColoredMulti
  ) where

import "base" Data.Int ( Int32 )
import "base" Data.Word ( Word8 )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types ( withPtr )
import "this" OpenCV.Internal.Exception
import "this" OpenCV.Internal.Photo.Constants
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Core.Types ( withArrayPtr )
import "this" OpenCV.TypeLevel

import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/photo.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

data InpaintingMethod
   = InpaintNavierStokes
     -- ^ Navier-Stokes based method.
   | InpaintTelea
     -- ^ Method by Alexandru Telea.
     deriving Show

marshalInpaintingMethod :: InpaintingMethod -> Int32
marshalInpaintingMethod = \case
  InpaintNavierStokes -> c'INPAINT_NS
  InpaintTelea        -> c'INPAINT_TELEA

{- | Restores the selected region in an image using the region neighborhood.

Example:

@
inpaintImg
    :: forall h h2 w w2 c d
     . ( Mat (ShapeT [h, w]) ('S c) ('S d) ~ Bikes_512x341
       , h2 ~ ((*) h 2)
       , w2 ~ ((*) w 2)
       )
    => Mat ('S ['S h2, 'S w2]) ('S c) ('S d)
inpaintImg = exceptError $ do
    maskInv <- bitwiseNot mask
    maskBgr <- cvtColor gray bgr maskInv
    damaged <- bitwiseAnd bikes_512x341 maskBgr
    repairedNS <- inpaint 3 InpaintNavierStokes damaged mask
    repairedT  <- inpaint 3 InpaintTelea        damaged mask
    withMatM
      (Proxy :: Proxy [h2, w2])
      (Proxy :: Proxy c)
      (Proxy :: Proxy d)
      black $ \\imgM -> do
        matCopyToM imgM (V2 0 0) damaged Nothing
        matCopyToM imgM (V2 w 0) maskBgr Nothing
        matCopyToM imgM (V2 0 h) repairedNS Nothing
        matCopyToM imgM (V2 w h) repairedT  Nothing
  where
    mask = damageMask

    w = fromInteger $ natVal (Proxy :: Proxy w)
    h = fromInteger $ natVal (Proxy :: Proxy h)
@

<<doc/generated/examples/inpaintImg.png inpaintImg>>
-}
inpaint
   :: (channels `In` [1, 3])
   => Double
      -- ^ inpaintRadius - Radius of a circular neighborhood of each
      -- point inpainted that is considered by the algorithm.
   -> InpaintingMethod
   -> Mat ('S [h, w]) ('S channels) ('S Word8) -- ^ Input image.
   -> Mat ('S [h, w]) ('S 1) ('S Word8) -- ^ Inpainting mask.
   -> CvExcept (Mat ('S [h, w]) ('S channels) ('S Word8)) -- ^ Output image.
inpaint inpaintRadius method src inpaintMask = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src         $ \srcPtr         ->
      withPtr inpaintMask $ \inpaintMaskPtr ->
      withPtr dst         $ \dstPtr         ->
      [cvExcept|
        cv::inpaint( *$(Mat * srcPtr)
                   , *$(Mat * inpaintMaskPtr)
                   , *$(Mat * dstPtr)
                   , $(double c'inpaintRadius)
                   , $(int32_t c'method)
                   );
      |]
  where
    c'method = marshalInpaintingMethod method
    c'inpaintRadius = realToFrac inpaintRadius

{- | Perform fastNlMeansDenoising function for colored images. Denoising is not
     per channel but in a different colour space

Example:

@
fastNlMeansDenoisingColoredImg
    :: forall h w w2 c d
     . ( Mat (ShapeT [h, w]) ('S c) ('S d) ~ Lenna_512x512
       , w2 ~ ((*) w 2)
       )
    => Mat ('S ['S h, 'S w2]) ('S c) ('S d)
fastNlMeansDenoisingColoredImg = exceptError $ do
    denoised <- fastNlMeansDenoisingColored 3 10 7 21 lenna_512x512
    withMatM
      (Proxy :: Proxy [h, w2])
      (Proxy :: Proxy c)
      (Proxy :: Proxy d)
      black $ \\imgM -> do
        matCopyToM imgM (V2 0 0) lenna_512x512 Nothing
        matCopyToM imgM (V2 w 0) denoised Nothing
  where
    w = fromInteger $ natVal (Proxy :: Proxy w)
@

<<doc/generated/examples/fastNlMeansDenoisingColoredImg.png fastNlMeansDenoisingColoredImg>>
-}

fastNlMeansDenoisingColored
   :: Double -- ^ Parameter regulating filter strength for luminance component.
             -- Bigger h value perfectly removes noise but also removes image
             -- details, smaller h value preserves details but also preserves
             -- some noise
   -> Double -- ^ The same as h but for color components. For most images value
             -- equals 10 will be enough to remove colored noise and do not
             -- distort colors
   -> Int32  -- ^ templateWindowSize Size in pixels of the template patch that
             -- is used to compute weights.
             -- Should be odd. Recommended value 7 pixels
   -> Int32  -- ^ searchWindowSize. Size in pixels of the window that is used
             -- to compute weighted average for given pixel. Should be odd.
             -- Affect performance linearly: greater searchWindowsSize
             -- - greater denoising time. Recommended value 21 pixels
   -> Mat ('S [h, w]) ('S 3) ('S Word8) -- ^ Input image 8-bit 3-channel image.
   -> CvExcept (Mat ('S [h, w]) ('S 3) ('S Word8))
             -- ^ Output image same size and type as input.
fastNlMeansDenoisingColored h hColor templateWindowSize searchWindowSize src =
  unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src         $ \srcPtr         ->
      withPtr dst         $ \dstPtr         ->
      [cvExcept|
        cv::fastNlMeansDenoisingColored( *$(Mat * srcPtr)
                                       , *$(Mat * dstPtr)
                                       , $(double c'h)
                                       , $(double c'hColor)
                                       , $(int32_t templateWindowSize)
                                       , $(int32_t searchWindowSize)
                                       );
      |]
  where
    c'h = realToFrac h
    c'hColor = realToFrac hColor

{- | Perform fastNlMeansDenoisingColoredMulti function for colored images.
     Denoising is not pre channel but in a different colour space.
     This wrapper differs from the original OpenCV version by using all input
     images and denoising the middle one. The original version would allow
     to have some arbitrary length vector and slide window over it. As we have
     to copy the haskell vector before we can use it as `std::vector` on the cpp
     side it is easier to trim the vector before sending and use all frames.

Example:

@
fastNlMeansDenoisingColoredMultiImg
    :: forall h w w2 c d
     . ( Mat (ShapeT [h, w]) ('S c) ('S d) ~ Lenna_512x512
       , w2 ~ ((*) w 2)
       )
    => Mat ('S ['S h, 'S w2]) ('S c) ('S d)
fastNlMeansDenoisingColoredMultiImg = exceptError $ do
    denoised <- fastNlMeansDenoisingColoredMulti 3 10 7 21 (V.singleton lenna_512x512)
    withMatM
      (Proxy :: Proxy [h, w2])
      (Proxy :: Proxy c)
      (Proxy :: Proxy d)
      black $ \\imgM -> do
        matCopyToM imgM (V2 0 0) lenna_512x512 Nothing
        matCopyToM imgM (V2 w 0) denoised Nothing
  where
    w = fromInteger $ natVal (Proxy :: Proxy w)
@

<<doc/generated/examples/fastNlMeansDenoisingColoredMultiImg.png fastNlMeansDenoisingColoredMultiImg>>
-}

fastNlMeansDenoisingColoredMulti
   :: Double -- ^ Parameter regulating filter strength for luminance component.
             -- Bigger h value perfectly removes noise but also removes image
             -- details, smaller h value preserves details but also preserves
             -- some noise
   -> Double -- ^ The same as h but for color components. For most images value
             -- equals 10 will be enough to remove colored noise and do not
             -- distort colors
   -> Int32  -- ^ templateWindowSize Size in pixels of the template patch that
             -- is used to compute weights. Should be odd.
             -- Recommended value 7 pixels
   -> Int32  -- ^ searchWindowSize. Size in pixels of the window that is used to
             -- compute weighted average for given pixel. Should be odd.
             -- Affect performance linearly: greater searchWindowsSize -
             -- greater denoising time. Recommended value 21 pixels
   -> V.Vector (Mat ('S [h, w]) ('S 3) ('S Word8))
             -- ^ Vector of odd number of input 8-bit 3-channel images.
   -> CvExcept (Mat ('S [h, w]) ('S 3) ('S Word8))
             -- ^ Output image same size and type as input.

fastNlMeansDenoisingColoredMulti h hColor templateWindowSize searchWindowSize srcVec =
  unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withArrayPtr srcVec $ \srcVecPtr      ->
      withPtr dst         $ \dstPtr         ->
      [cvExcept|
        std::vector<Mat> buffer( $(Mat * srcVecPtr)
                    , $(Mat * srcVecPtr) + $(int32_t c'temporalWindowSize) );
        cv::fastNlMeansDenoisingColoredMulti( buffer
                                            , *$(Mat * dstPtr)
                                            , $(int32_t c'imgToDenoiseIndex)
                                            , $(int32_t c'temporalWindowSize)
                                            , $(double c'h)
                                            , $(double c'hColor)
                                            , $(int32_t templateWindowSize)
                                            , $(int32_t searchWindowSize)
                                            );
      |]
  where
    c'h = realToFrac h
    c'hColor = realToFrac hColor
    c'srcVecLength = fromIntegral $ V.length srcVec
    -- if it is not odd we drop the last image
    c'temporalWindowSize
        | c'srcVecLength `mod` 2 == 1 = c'srcVecLength
        | otherwise                   = c'srcVecLength - 1
    c'imgToDenoiseIndex = (c'temporalWindowSize - 1) `div` 2

{- | Perform denoise_TVL1

Example:

@

denoise_TVL1Img
    :: forall h w w2 c d
     . ( Mat (ShapeT [h, w]) ('S c) ('S d) ~ Lenna_512x512
       , w2 ~ ((*) w 2)
       )
    => Mat ('S ['S h, 'S w2]) ('S c) ('S d)
denoise_TVL1Img = exceptError $ do
    denoised <- matChannelMapM (denoise_TVL1 2 50 . V.singleton) lenna_512x512
    withMatM
      (Proxy :: Proxy [h, w2])
      (Proxy :: Proxy c)
      (Proxy :: Proxy d)
      black $ \\imgM -> do
        matCopyToM imgM (V2 0 0) lenna_512x512 Nothing
        matCopyToM imgM (V2 w 0) denoised Nothing
  where
    w = fromInteger $ natVal (Proxy :: Proxy w)
@

<<doc/generated/examples/denoise_TVL1Img.png denoise_TVL1Img>>
-}

denoise_TVL1
   :: Double -- ^ details more is more 2
   -> Int32  -- ^ Number of iterations that the algorithm will run
   -> V.Vector (Mat ('S [h, w]) ('S 1) ('S Word8))
             -- ^ Vector of odd number of input 8-bit 3-channel images.
   -> CvExcept (Mat ('S [h, w]) ('S 1) ('S Word8))
             -- ^ Output image same size and type as input.

denoise_TVL1 lambda niters srcVec = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withArrayPtr srcVec $ \srcVecPtr      ->
      withPtr dst         $ \dstPtr         ->
      [cvExcept|
        std::vector<Mat> buffer( $(Mat * srcVecPtr)
                           , $(Mat * srcVecPtr) + $(int32_t c'srcVecLength) );
        cv::denoise_TVL1( buffer
                        , *$(Mat * dstPtr)
                        , $(double c'lambda)
                        , $(int32_t niters)
                        );
      |]
  where
    c'lambda = realToFrac lambda
    c'srcVecLength = fromIntegral $ V.length srcVec


{- | Perform decolor

Decolor a color image to a grayscale (1 channel) and a color boosted image (3 channel)

Example:

@
decolorImg
    :: forall h h2 w w2 c d
     . ( Mat (ShapeT [h, w]) ('S c) ('S d) ~ Bikes_512x341
       , h2 ~ ((*) h 2)
       , w2 ~ ((*) w 2)
       )
    => Mat ('S ['S h2, 'S w2]) ('S c) ('S d)
decolorImg = exceptError $ do
    (bikesGray, boost) <- decolor bikes_512x341
    colorGray <- cvtColor gray bgr bikesGray
    withMatM
      (Proxy :: Proxy [h2, w2])
      (Proxy :: Proxy c)
      (Proxy :: Proxy d)
      white $ \\imgM -> do
        matCopyToM imgM (V2 0 0) bikes_512x341 Nothing
        matCopyToM imgM (V2 0 h) colorGray Nothing
        matCopyToM imgM (V2 w h) boost  Nothing
  where
    w = fromInteger $ natVal (Proxy :: Proxy w)
    h = fromInteger $ natVal (Proxy :: Proxy h)
@

<<doc/generated/examples/decolorImg.png decolorImg>>
-}

decolor
   :: Mat ('S [h, w]) ('S 3) ('S Word8) -- ^ Input image.
   -> CvExcept (Mat ('S [h, w]) ('S 1) ('S Word8), Mat ('S [h, w]) ('S 3) ('S Word8)) -- ^ Output images.

decolor src = unsafeWrapException $ do
    gray <- newEmptyMat
    boost <- newEmptyMat

    handleCvException (pure (unsafeCoerceMat gray, unsafeCoerceMat boost)) $
      withPtr src         $ \srcPtr         ->
      withPtr gray        $ \grayPtr        ->
      withPtr boost       $ \boostPtr       ->
      [cvExcept|
        cv::decolor( *$(Mat * srcPtr)
                   , *$(Mat * grayPtr)
                   , *$(Mat * boostPtr)
                   );
      |]
