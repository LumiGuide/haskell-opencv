{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
module OpenCV.ImgProc.CascadeClassifier
  ( CascadeClassifier
  , newCascadeClassifier
  , cascadeClassifierDetectMultiScale
  ) where

import "base" Data.Int
import "base" Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import "base" Foreign.C.String (withCString)
import "base" System.IO.Unsafe (unsafePerformIO)
import "base" Data.Word
import "base" Foreign.Marshal.Alloc (alloca)
import "base" Foreign.Ptr (Ptr)
import "base" Control.Exception (mask_)
import "base" Foreign.Storable (peek)
import "base" Foreign.Marshal.Array (peekArray)
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import qualified "vector" Data.Vector as V
import "linear" Linear (V2(..))
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal
import "this" OpenCV.TypeLevel

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/objdetect.hpp"
C.using "namespace cv"

newtype CascadeClassifier = CascadeClassifier {unCascadeClassifier :: ForeignPtr (C CascadeClassifier)}

type instance C CascadeClassifier = C'CascadeClassifier

instance WithPtr CascadeClassifier where
    withPtr = withForeignPtr . unCascadeClassifier

instance FromPtr CascadeClassifier where
    fromPtr = objFromPtr CascadeClassifier $ \ptr ->
                [CU.exp| void { delete $(CascadeClassifier * ptr) }|]

-- | Create a new cascade classifier. Returns 'Nothing' if the classifier
-- is empty after initialization. This usually means that the file could
-- not be loaded (e.g. it doesn't exist, is corrupt, etc.)
newCascadeClassifier :: FilePath -> IO (Maybe CascadeClassifier)
newCascadeClassifier fp = do
  cc <- withCString fp $ \c'fp -> fromPtr
    [CU.exp| CascadeClassifier * { new CascadeClassifier(cv::String($(const char * c'fp))) } |]
  -- TODO: empty() seems to return bogus numbers when the classifier is not
  -- empty, and I'm not sure why. This is also why I'm not using toBool.
  empty <- fmap (== 1) (withPtr cc (\ccPtr -> [CU.exp| bool { $(CascadeClassifier * ccPtr)->empty() } |]))
  return $ if empty
    then Nothing
    else Just cc

cascadeClassifierDetectMultiScale ::
     (IsSize size Int32)
  => CascadeClassifier
  -> Maybe Double -- ^ Scale factor, default is 1.1
  -> Maybe Int32 -- ^ Min neighbours, default 3
  -> Maybe (size Int32) -- ^ Minimum size. Default: no minimum.
  -> Maybe (size Int32) -- ^ Maximum size. Default: no maximum.
  -> Mat ('S [w, h]) ('S 1) ('S Word8)
  -> V.Vector (Rect Int32)
cascadeClassifierDetectMultiScale cc scaleFactor minNeighbours minSize maxSize src = unsafePerformIO $
    withPtr cc $ \ccPtr ->
    withPtr src $ \srcPtr ->
    withPtr c'minSize $ \minSizePtr ->
    withPtr c'maxSize $ \maxSizePtr ->
    alloca $ \(numRectsPtr :: Ptr Int32) ->
    alloca $ \(rectsPtrPtr :: Ptr (Ptr (Ptr (C'Rect Int32)))) -> mask_ $ do
      [CU.block| void {
        std::vector<cv::Rect> rects;
        $(CascadeClassifier * ccPtr)->detectMultiScale(
          *$(Mat * srcPtr),
          rects,
          $(double c'scaleFactor),
          $(int32_t c'minNeighbours),
          0,
          *$(Size2i * minSizePtr),
          *$(Size2i * maxSizePtr));
        *$(int32_t * numRectsPtr) = rects.size();
        cv::Rect * * rectsPtr = new cv::Rect * [rects.size()];
        *$(Rect2i * * * rectsPtrPtr) = rectsPtr;
        for (std::vector<cv::Rect>::size_type i = 0; i != rects.size(); i++) {
          rectsPtr[i] = new cv::Rect(rects[i]);
        }
      } |]
      numRects <- fromIntegral <$> peek numRectsPtr
      rectsPtr <- peek rectsPtrPtr
      rects :: [Rect Int32] <- peekArray numRects rectsPtr >>= mapM (fromPtr . return)
      [CU.block| void { delete [] *$(Rect2i * * * rectsPtrPtr); }|]
      return (V.fromList rects)
  where
    c'scaleFactor = maybe 1.1 realToFrac scaleFactor
    c'minNeighbours = maybe 3 fromIntegral minNeighbours
    c'minSize = maybe (toSize (V2 0 0)) toSize minSize
    c'maxSize = maybe (toSize (V2 0 0)) toSize maxSize
