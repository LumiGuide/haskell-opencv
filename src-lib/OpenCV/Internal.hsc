{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Internal where

import "base" Foreign.C.String ( peekCString )
import "base" Foreign.Concurrent ( newForeignPtr )
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Alloc ( allocaBytes )
import "base" Foreign.Marshal.Array ( allocaArray )
import "base" Foreign.Ptr ( Ptr, nullPtr, plusPtr )
import "base" Foreign.Storable ( sizeOf, poke )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude
import "template-haskell" Language.Haskell.TH.Quote ( QuasiQuoter, quoteExp )
import "this" Language.C.Inline.OpenCV
import qualified "vector" Data.Vector as V


--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"

#include "namespace.hpp"
#include "macros.hpp"

#sizeof Point2i


--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

newtype CvException = CvException { unCvException :: ForeignPtr C'Exception }

instance Exception CvException

instance Show CvException where
    show cvException = unsafePerformIO $
        withCvExceptionPtr cvException $ \cvExceptionPtr -> do
          charPtr <- [CU.exp| const char * { $(Exception * cvExceptionPtr)->what() } |]
          peekCString charPtr

withCvExceptionPtr :: CvException -> (Ptr C'Exception -> IO a) -> IO a
withCvExceptionPtr = withForeignPtr . unCvException

cvExceptionFromPtr :: IO (Ptr C'Exception) -> IO CvException
cvExceptionFromPtr = objFromPtr CvException $ \ptr -> [CU.exp| void { delete $(Exception * ptr) }|]

handleCvException :: a -> IO (Ptr C'Exception) -> IO (Either CvException a)
handleCvException okResult act = mask_ $ do
    exceptionPtr <- act
    if exceptionPtr /= nullPtr
      then Left <$> cvExceptionFromPtr (pure exceptionPtr)
      else pure $ Right okResult

cvExcept :: QuasiQuoter
cvExcept = C.block {quoteExp = \s -> quoteExp C.block $ cvExceptWrap s}

cvExceptU :: QuasiQuoter
cvExceptU = CU.block {quoteExp = \s -> quoteExp CU.block $ cvExceptWrap s}

cvExceptWrap :: String -> String
cvExceptWrap s =
    "Exception * {\n\
    \  try\n\
    \  {\n   " <> s <> "\n\
    \    return NULL;\n\
    \  }\n\
    \  catch (const cv::Exception & e)\n\
    \  {\n\
    \    return new cv::Exception(e);\n\
    \  }\n\
    \}"

--------------------------------------------------------------------------------
--  Point
--------------------------------------------------------------------------------

-- | 2D point with 'Int'eger coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point OpenCV doc>
newtype Point2i = Point2i {unPoint2i :: ForeignPtr C'Point2i}
-- | 2D point with 'Int'eger coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point OpenCV doc>
type Point = Point2i
-- | 2D point with 'Float' coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point OpenCV doc>
newtype Point2f = Point2f {unPoint2f :: ForeignPtr C'Point2f}
-- | 2D point with 'Double' coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point OpenCV doc>
newtype Point2d = Point2d {unPoint2d :: ForeignPtr C'Point2d}
-- | 3D point
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point3 OpenCV doc>
newtype Point3i = Point3i {unPoint3i :: ForeignPtr C'Point3i}
-- | 3D point
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point3 OpenCV doc>
newtype Point3f = Point3f {unPoint3f :: ForeignPtr C'Point3f}
-- | 3D point
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point3 OpenCV doc>
newtype Point3d = Point3d {unPoint3d :: ForeignPtr C'Point3d}

point2iFromPtr :: IO (Ptr C'Point2i) -> IO Point2i
point2fFromPtr :: IO (Ptr C'Point2f) -> IO Point2f
point2dFromPtr :: IO (Ptr C'Point2d) -> IO Point2d
point3iFromPtr :: IO (Ptr C'Point3i) -> IO Point3i
point3fFromPtr :: IO (Ptr C'Point3f) -> IO Point3f
point3dFromPtr :: IO (Ptr C'Point3d) -> IO Point3d

point2iFromPtr = objFromPtr Point2i $ \ptr -> [CU.exp| void { delete $(Point2i * ptr) }|]
point2fFromPtr = objFromPtr Point2f $ \ptr -> [CU.exp| void { delete $(Point2f * ptr) }|]
point2dFromPtr = objFromPtr Point2d $ \ptr -> [CU.exp| void { delete $(Point2d * ptr) }|]
point3iFromPtr = objFromPtr Point3i $ \ptr -> [CU.exp| void { delete $(Point3i * ptr) }|]
point3fFromPtr = objFromPtr Point3f $ \ptr -> [CU.exp| void { delete $(Point3f * ptr) }|]
point3dFromPtr = objFromPtr Point3d $ \ptr -> [CU.exp| void { delete $(Point3d * ptr) }|]

withPoint2iPtr :: Point2i -> (Ptr C'Point2i -> IO a) -> IO a
withPoint2fPtr :: Point2f -> (Ptr C'Point2f -> IO a) -> IO a
withPoint2dPtr :: Point2d -> (Ptr C'Point2d -> IO a) -> IO a
withPoint3iPtr :: Point3i -> (Ptr C'Point3i -> IO a) -> IO a
withPoint3fPtr :: Point3f -> (Ptr C'Point3f -> IO a) -> IO a
withPoint3dPtr :: Point3d -> (Ptr C'Point3d -> IO a) -> IO a

withPoint2iPtr = withForeignPtr . unPoint2i
withPoint2fPtr = withForeignPtr . unPoint2f
withPoint2dPtr = withForeignPtr . unPoint2d
withPoint3iPtr = withForeignPtr . unPoint3i
withPoint3fPtr = withForeignPtr . unPoint3f
withPoint3dPtr = withForeignPtr . unPoint3d

withPolygons
    :: forall a
     . V.Vector (V.Vector Point2i)
    -> (Ptr (Ptr C'Point2i) -> IO a)
    -> IO a
withPolygons polygons act =
    allocaArray (V.length polygons) $ \polygonsPtr -> do
      let go :: Ptr (Ptr C'Point2i) -> Int -> IO a
          go !acc !ix
            | ix < V.length polygons =
                withPoints (V.unsafeIndex polygons ix) $ \ptsPtr -> do
                  poke acc ptsPtr
                  go (acc `plusPtr` sizeOf (undefined :: Ptr (Ptr C'Point2i))) (ix + 1)
            | otherwise = act polygonsPtr
      go polygonsPtr 0

withPoints
    :: V.Vector Point2i
    -> (Ptr C'Point2i -> IO a)
    -> IO a
withPoints points act =
    allocaBytes (c'sizeof_Point2i * V.length points) $ \ptsPtr -> do
      V.foldM'_ copyNext ptsPtr points
      act ptsPtr
  where
    copyNext :: Ptr C'Point2i -> Point2i -> IO (Ptr C'Point2i)
    copyNext !ptr point = copyPoint2i ptr point $> plusPtr ptr c'sizeof_Point2i

    copyPoint2i :: Ptr C'Point2i -> Point2i -> IO ()
    copyPoint2i destPtr src =
      withPoint2iPtr src $ \srcPtr ->
        [C.exp| void { new($(Point2i * destPtr)) Point2i(*$(Point2i * srcPtr)) }|]


--------------------------------------------------------------------------------
--  Size
--------------------------------------------------------------------------------

-- | Size of an image or rectangle.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#size OpenCV doc>
newtype Size2i = Size2i {unSize2i :: ForeignPtr C'Size2i}
-- | Size of an image or rectangle.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#size OpenCV doc>
type Size = Size2i
-- | Size of an image or rectangle.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#size OpenCV doc>
newtype Size2f = Size2f {unSize2f :: ForeignPtr C'Size2f}

size2iFromPtr :: IO (Ptr C'Size2i) -> IO Size2i
size2fFromPtr :: IO (Ptr C'Size2f) -> IO Size2f

size2iFromPtr = objFromPtr Size2i $ \ptr -> [CU.exp| void { delete $(Size2i * ptr) }|]
size2fFromPtr = objFromPtr Size2f $ \ptr -> [CU.exp| void { delete $(Size2f * ptr) }|]

withSize2iPtr :: Size2i -> (Ptr C'Size2i -> IO a) -> IO a
withSize2fPtr :: Size2f -> (Ptr C'Size2f -> IO a) -> IO a

withSize2iPtr = withForeignPtr . unSize2i
withSize2fPtr = withForeignPtr . unSize2f


--------------------------------------------------------------------------------
--  Rect
--------------------------------------------------------------------------------

-- | 2D rectangles
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#rect OpenCV doc>
newtype Rect = Rect {unRect :: ForeignPtr C'Rect}

rectFromPtr :: IO (Ptr C'Rect) -> IO Rect
rectFromPtr = objFromPtr Rect $ \ptr -> [CU.exp| void { delete $(Rect * ptr) }|]

withRectPtr :: Rect -> (Ptr C'Rect -> IO a) -> IO a
withRectPtr = withForeignPtr . unRect


--------------------------------------------------------------------------------
--  RotatedRect
--------------------------------------------------------------------------------

-- | Rotated (i.e. not up-right) rectangles on a plane. Each rectangle
-- is specified by the center point (mass center), length of each side
-- (represented by 'Size2f') and the rotation angle in degrees.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#rotatedrect OpenCV doc>
newtype RotatedRect = RotatedRect {unRotatedRect :: ForeignPtr C'RotatedRect}

rotatedRectFromPtr :: IO (Ptr C'RotatedRect) -> IO RotatedRect
rotatedRectFromPtr = objFromPtr RotatedRect $ \ptr -> [CU.exp| void { delete $(RotatedRect * ptr) }|]

withRotatedRectPtr :: RotatedRect -> (Ptr C'RotatedRect -> IO a) -> IO a
withRotatedRectPtr = withForeignPtr . unRotatedRect


--------------------------------------------------------------------------------
--  TermCriteria
--------------------------------------------------------------------------------

-- | Termination criteria for iterative algorithms
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#termcriteria OpenCV doc>
newtype TermCriteria = TermCriteria { unTermCriteria :: ForeignPtr C'TermCriteria }

termCriteriaFromPtr :: IO (Ptr C'TermCriteria) -> IO TermCriteria
termCriteriaFromPtr = objFromPtr TermCriteria $ \ptr -> [CU.exp| void { delete $(TermCriteria * ptr) }|]

withTermCriteriaPtr :: TermCriteria -> (Ptr C'TermCriteria -> IO a) -> IO a
withTermCriteriaPtr = withForeignPtr . unTermCriteria


--------------------------------------------------------------------------------
--  Scalar
--------------------------------------------------------------------------------

newtype Scalar = Scalar {unScalar :: ForeignPtr C'Scalar}

scalarFromPtr :: IO (Ptr C'Scalar) -> IO Scalar
scalarFromPtr = objFromPtr Scalar $ \ptr -> [CU.exp| void { delete $(Scalar * ptr) }|]

withScalarPtr :: Scalar -> (Ptr C'Scalar -> IO a) -> IO a
withScalarPtr = withForeignPtr . unScalar


--------------------------------------------------------------------------------
--  Matrix
--------------------------------------------------------------------------------

newtype Mat = Mat {unMat :: ForeignPtr C'Mat}

matFromPtr :: IO (Ptr C'Mat) -> IO Mat
matFromPtr = objFromPtr Mat $ \ptr -> [CU.exp| void { delete $(Mat * ptr) }|]

withMatPtr :: Mat -> (Ptr C'Mat -> IO a) -> IO a
withMatPtr = withForeignPtr . unMat


--------------------------------------------------------------------------------
-- Mutable Matrix
--------------------------------------------------------------------------------

newtype MutMat s = MutMat { unMutMat :: Mat }


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

objFromPtr :: (ForeignPtr c -> hask) -> (Ptr c -> IO ()) -> IO (Ptr c) -> IO hask
objFromPtr haskCons finalizer mkObjPtr = mask_ $ do
    objPtr <- mkObjPtr
    haskCons <$> newForeignPtr objPtr (finalizer objPtr)
