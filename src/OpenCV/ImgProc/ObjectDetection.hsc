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
import "base" GHC.TypeLits
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
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

-- | <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/object_detection.html#matchtemplate OpenCV Sphinx doc>
data MatchTemplateMethod
   = MatchTemplateSqDiff
       -- ^ * not normed: <<http://docs.opencv.org/3.0-last-rst/_images/math/f096a706cb9499736423f10d901c7fe13a1e6926.png>>
       --   * normed: <<http://docs.opencv.org/3.0-last-rst/_images/math/6d6a720237b3a4c1365c8e86a9cfcf0895d5e265.png>>
   | MatchTemplateCCorr
       -- ^ * not normed: <<http://docs.opencv.org/3.0-last-rst/_images/math/93f1747a86a3c5095a0e6a187442c6e2a0ae0968.png>>
       --   * normed: <<http://docs.opencv.org/3.0-last-rst/_images/math/6a72ad9ae17c4dad88e33ed16308fc1cfba549b8.png>>
   | MatchTemplateCCoeff
       -- ^ * not normed: <<http://docs.opencv.org/3.0-last-rst/_images/math/c9b62df96d0692d90cc1d8a5912a68a44461910c.png>>
       --   * where <<http://docs.opencv.org/3.0-last-rst/_images/math/ffb6954b6020b02e13b73c79bd852c1627cfb79c.png>>
       --   * normed: <<http://docs.opencv.org/3.0-last-rst/_images/math/235e42ec68d2d773899efcf0a4a9d35a7afedb64.png>>
     deriving Show

-- | Whether to use normalisation. See 'MatchTemplateMethod'.
data MatchTemplateNormalisation
   = MatchTemplateNotNormed
   | MatchTemplateNormed
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

-- | <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/object_detection.html#matchtemplate OpenCV Sphinx doc>
--
-- Compares a template against overlapped image regions.
--
-- The function slides through image, compares the overlapped patches
-- of size
-- <<http://docs.opencv.org/3.0-last-rst/_images/math/d47153257f0243694e5632bb23b85009eb9e5599.png w \times h>>
-- against templ using the specified method and stores the comparison
-- results in result . Here are the formulae for the available
-- comparison methods
-- (<<http://docs.opencv.org/3.0-last-rst/_images/math/06f9f0fcaa8d96a6a23b0f7d1566fe5efaa789ad.png I>> denotes image,
-- <<http://docs.opencv.org/3.0-last-rst/_images/math/87804527283a4539e1e17c5861df8cb92a97fd6d.png T>> template,
-- <<http://docs.opencv.org/3.0-last-rst/_images/math/8fa391da5431a5d6eaba1325c3e7cb3da22812b5.png R>> result).
-- The summation is done over template and/or the image patch:
-- <<http://docs.opencv.org/3.0-last-rst/_images/math/ff90cafd4a71d85875237787b54815ee8ac77bff.png x' = 0...w-1, y' = 0...h-1>>
matchTemplate
    :: ( depth `In` [Word8, Float]
       , Length searchShape <= 2
       )
    => Mat ('S searchShape) ('S 1) ('S depth)
       -- ^ Image where the search is running. It must be 8-bit or 32-bit floating-point.
    -> Mat ('S [th, tw]) ('S 1) ('S depth)
       -- ^ Searched template. It must be not greater than the source image and have the same data type.
    -> MatchTemplateMethod
       -- ^ Parameter specifying the comparison method.
    -> MatchTemplateNormalisation
       -- ^ Normalise
    -> CvExcept (Mat ('S [rh, rw]) ('S 1) ('S Float))
       -- ^ Map of comparison results. It must be single-channel 32-bit floating-point.
       -- If image is
       -- <<http://docs.opencv.org/3.0-last-rst/_images/math/e4926c3d97c3f7434c6317ba24b8b9294a0aba64.png>>
       -- and templ is
       -- <<http://docs.opencv.org/3.0-last-rst/_images/math/d47153257f0243694e5632bb23b85009eb9e5599.png>>
       -- , then result is
       -- <<http://docs.opencv.org/3.0-last-rst/_images/math/e318d7237b57e08135e689fd9136b9ac8e4a4102.png>>.
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
