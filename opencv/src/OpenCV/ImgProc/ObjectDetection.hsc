{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#if __GLASGOW_HASKELL__ >= 800
{-# options_ghc -Wno-redundant-constraints #-}
#endif

module OpenCV.ImgProc.ObjectDetection
    ( MatchTemplateMethod(..)
    , MatchTemplateNormalisation(..)
    , matchTemplate
    ) where

import "base" Data.Int
import "base" Data.Word
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "mtl" Control.Monad.Error.Class ( MonadError )
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Exception
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.include "opencv2/objdetect.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"

#include "namespace.hpp"
#include "hsc_macros.hpp"

--------------------------------------------------------------------------------

-- | Type of the template matching operation
--
-- In the formulae for the comparison methods \(\bf{I}\) denotes image,
-- \(\bf{T}\) template and \(\bf{R}\) result. Each method supports
-- normalization. See 'MatchTemplateNormalisation'.
data MatchTemplateMethod
   = MatchTemplateSqDiff
       {- ^
       * not normed:
           \[
           R(x,y) = \sum _{x',y'} (T(x',y')-I(x+x',y+y'))^2
           \]

       * normed:
           \[
           R(x,y) = \frac{\sum_{x',y'} (T(x',y')-I(x+x',y+y'))^2}
                         {\sqrt{\sum_{x',y'}T(x',y')^2 \cdot \sum_{x',y'} I(x+x',y+y')^2}}
           \]
       -}
   | MatchTemplateCCorr
       {- ^
       * not normed:
           \[
           R(x,y) = \sum _{x',y'} (T(x',y')  \cdot I(x+x',y+y'))
           \]

       * normed:
           \[
           R(x,y) = \frac{\sum_{x',y'} (T(x',y') \cdot I(x+x',y+y'))}
                         {\sqrt{\sum_{x',y'}T(x',y')^2 \cdot \sum_{x',y'} I(x+x',y+y')^2}}
           \]
       -}
   | MatchTemplateCCoeff
       {- ^
       * not normed:
           \[
           R(x,y) = \sum _{x',y'} (T'(x',y') \cdot I'(x+x',y+y'))
           \]

       * where
           \[ \begin{array}{l}
           T'(x',y') = T(x',y') - 1/(w \cdot h) \cdot \sum _{x'',y''} T(x'',y'')
           \\ I'(x+x',y+y') = I(x+x',y+y') - 1/(w \cdot h) \cdot \sum _{x'',y''} I(x+x'',y+y'')
           \end{array} \]

       * normed:
           \[
           R(x,y) = \frac{ \sum_{x',y'} (T'(x',y') \cdot I'(x+x',y+y')) }
                         { \sqrt{\sum_{x',y'}T'(x',y')^2 \cdot \sum_{x',y'} I'(x+x',y+y')^2} }
           \]
       -}
     deriving Show

-- | Whether to use normalisation. See 'MatchTemplateMethod'.
data MatchTemplateNormalisation
   = MatchTemplateNotNormed -- ^ Do not use normalization.
   | MatchTemplateNormed    -- ^ Use normalization.
   deriving (Show, Eq)

#num CV_TM_SQDIFF
#num CV_TM_SQDIFF_NORMED
#num CV_TM_CCORR
#num CV_TM_CCORR_NORMED
#num CV_TM_CCOEFF
#num CV_TM_CCOEFF_NORMED

marshalMatchTemplateMethod :: MatchTemplateMethod -> Bool -> Int32
marshalMatchTemplateMethod m n =
    case (m, n) of
      (MatchTemplateSqDiff, False) -> c'CV_TM_SQDIFF
      (MatchTemplateSqDiff, True ) -> c'CV_TM_SQDIFF_NORMED
      (MatchTemplateCCorr , False) -> c'CV_TM_CCORR
      (MatchTemplateCCorr , True ) -> c'CV_TM_CCORR_NORMED
      (MatchTemplateCCoeff, False) -> c'CV_TM_CCOEFF
      (MatchTemplateCCoeff, True ) -> c'CV_TM_CCOEFF_NORMED

{- | Compares a template against overlapped image regions.

The function slides through image, compares the overlapped patches of size
  \( w \times h \)
against templ using the specified method and stores the comparison
results in result. The summation is done over template and/or the image patch:
  \( x' = 0...w-1, y' = 0...h-1 \)

After the function finishes the comparison, the best matches can be found as
global minimums (when 'MatchTemplateSqDiff' was used) or maximums (when
'MatchTemplateCCorr' or 'MatchTemplateCCoeff' was used) using the 'minMaxLoc'
function. In case of a color image, template summation in the numerator and each
sum in the denominator is done over all of the channels and separate mean values
are used for each channel.  That is, the function can take a color template and
a color image. The result will still be a single-channel image, which is easier
to analyze.

Example:

@
matchTemplateImg
    :: forall (width :: Nat) (height :: Nat) (width2 :: Nat)
     . ( Mat (ShapeT [height, width]) ('S 3) ('S Word8) ~ Kodak_512x341
       , width2 ~ (width + width)
       )
    => Mat (ShapeT [height, width2]) ('S 3) ('S Word8)
matchTemplateImg = exceptError $
    withMatM (Proxy :: Proxy [height, width2])
             (Proxy :: Proxy 3)
             (Proxy :: Proxy Word8)
             transparent $ \\imgM -> do
      matCopyToM imgM (V2 0 0) barn_512x341 Nothing
      rectangle imgM templateRect blue 1 LineType_8 0
      matCopyToM imgM (V2 width 0) resultImg Nothing
      rectangle imgM matchRect blue 1 LineType_8 0
  where
    -- Recovered location of 'template', translated for rendering.
    matchRect :: Rect2i
    matchRect = toRect $ HRect (fromPoint maxLoc ^+^ V2 width 0)
                               (V2 20 20)

    -- Find location of best match in 'result'.
    _minVal, _maxVal :: Double
    _minLoc, maxLoc :: Point2i
    (_minVal, _maxVal, _minLoc, maxLoc) = exceptError $ minMaxLoc result

    -- Result matrix converted to color image for rendering.
    resultImg :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
    resultImg = exceptError $ do
        resultGray
            :: Mat ('S ['D, 'D]) ('S 1) ('S Word8)
            <- matConvertTo (Just 255) Nothing result
        cvtColor gray bgr resultGray

    -- Result of looking for 'template' in 'barn_512x341'.
    result :: Mat ('S ['D, 'D]) ('S 1) ('S Float)
    result = exceptError $
        matchTemplate barn_512x341 template MatchTemplateCCoeff MatchTemplateNormed

    -- Small part of the barn image which we want to find again.
    template :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
    template = exceptError $ matSubRect barn_512x341 templateRect

    -- Rectangle that defines a small part of the barn image.
    templateRect :: Rect2i
    templateRect = toRect $ HRect (V2 183 24) (V2 20 20)

    width :: Int32
    width = fromInteger $ natVal (Proxy :: Proxy width)
@

<<doc/generated/examples/matchTemplateImg.png matchTemplateImg>>
-}
matchTemplate
    :: (depth `In` [Word8, Float], MonadError CvException m)
    => Mat ('S [sh, sw]) ('S channels) ('S depth)
       -- ^ Image where the search is running. It must be 8-bit or 32-bit floating-point.
    -> Mat ('S [th, tw]) ('S channels) ('S depth)
       -- ^ Searched template. It must be not greater than the source image and have the same data type.
    -> MatchTemplateMethod -- ^ Comparison method.
    -> MatchTemplateNormalisation -- ^ Normalization.
    -> m (Mat ('S [rh, rw]) ('S 1) ('S Float))
       -- ^ Map of comparison results. It must be single-channel 32-bit
       -- floating-point. If image is \(W \times H\) and templ is
       -- \(w \times h\), then result is \((W-w+1) \times (H-h+1)\).
matchTemplate image templ method normalisation = unsafeWrapException $ do
    result <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat result) $
      withPtr result $ \resultPtr ->
      withPtr image $ \imagePtr ->
      withPtr templ $ \templPtr ->
        [cvExcept|
          cv::matchTemplate( *$(Mat * imagePtr)
                           , *$(Mat * templPtr)
                           , *$(Mat * resultPtr)
                           , $(int32_t c'method)
                           );
        |]
  where
    normed =
      case normalisation of
        MatchTemplateNotNormed -> False
        MatchTemplateNormed -> True
    c'method = marshalMatchTemplateMethod method normed
