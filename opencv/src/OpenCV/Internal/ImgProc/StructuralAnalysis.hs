{-# language CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.ImgProc.StructuralAnalysis
    ( approxPolyDP
    , ApproxPolyDP(..)
    , boundingRect
    , BoundingRect(..)
    , convexHull
    , ConvexHull(..)
    , convexHullIndices
    , ConvexHullIndices(..)
    ) where

import "base" Data.Int
import "base" Foreign.C.Types
import "base" Foreign.Concurrent ( newForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import "mtl" Control.Monad.Error.Class ( MonadError, throwError )
import "this" OpenCV.Core.Types.Point
import "this" OpenCV.Core.Types.Rect ( HRect(..), Rect2i, toRectIO )
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.PlacementNew ( PlacementNew )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types
import "this" OpenCV.Internal.Exception
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Storable as VS

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------


{- | Approximates a polygonal curve(s) with the specified precision.

The functions approxPolyDP approximate a curve or a polygon with another
curve/polygon with less vertices so that the distance between them is less or
equal to the specified precision. It uses the
<http://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm Douglas-Peucker algorithm>

Example:

@
approxHandContourImg
    :: forall (width    :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *  )
     . (Mat ('S ['S height, 'S width]) ('S channels) ('S depth) ~ Hand)
    => IO (Mat ('S ['S height, 'S width]) ('S channels) ('S depth))
approxHandContourImg = do
    handContour <- mkHandContour
    let approx1 = exceptError $ approxPolyDP handContour 10 True
        approx2 = exceptError $ approxPolyDP handContour 25 True
    pure $ exceptError $
      withMatM (Proxy :: Proxy [height, width])
               (Proxy :: Proxy channels)
               (Proxy :: Proxy depth)
               transparent $ \\imgM -> do
        polylines imgM (V.singleton handContour) True blue  1 LineType_AA 0
        polylines imgM (V.singleton approx1)     True red   1 LineType_AA 0
        polylines imgM (V.singleton approx2)     True green 1 LineType_AA 0
@

<<doc/generated/examples/approxHandContourImg.png approxHandContourImg>>
-}
approxPolyDP
    :: forall point2 depth m
     . ( IsPoint2 point2 depth
       , ApproxPolyDP depth
       , MonadError CvException m
       )
    => V.Vector (point2 depth)
    -> Double -- ^ epsilon
    -> Bool   -- ^ is closed
    -> m (V.Vector (Point 2 depth))
approxPolyDP curve epsilon isClosed
    | V.null curve = pure V.empty
    | otherwise =
        unsafeWrapException $
        unsafeWithArrayPtr (V.map toPoint curve) $ \curvePtr ->
        alloca $ \(approxPtrPtr :: Ptr (Ptr (Ptr (C (Point 2 depth))))) ->
        alloca $ \(approxSizePtr :: Ptr C.CSize) ->
        handleCvException
          ( do approxSize <- fromIntegral <$> peek approxSizePtr
               approxPtr :: Ptr (Ptr (C (Point 2 depth))) <- peek approxPtrPtr
               approxList :: [Ptr (C (Point 2 depth))] <- peekArray approxSize approxPtr
               approxVec <- V.fromList <$> mapM (fromPtr . pure) approxList
               approxPolyDP_deletePtrArray approxPtrPtr
               pure approxVec
          ) $
          approxPolyDP_internal
            (fromIntegral $ V.length curve)
            curvePtr
            approxSizePtr
            approxPtrPtr
            (toCDouble epsilon)
            (fromIntegral (fromBool isClosed))

-- | Internal class used to overload the 'approxPolyDP' depth.
class ( FromPtr      (Point   2 depth)
      , CSizeOf      (C'Point 2 depth)
      , PlacementNew (C'Point 2 depth)
      ) => ApproxPolyDP depth where
    approxPolyDP_internal
        :: Int32 -- ^ Number of input curve points.
        -> Ptr (C (Point 2 depth)) -- ^ Input curve points array.
        -> Ptr C.CSize -- ^ Size of approximated curve.
        -> Ptr (Ptr (Ptr (C (Point 2 depth))))
           -- ^ Array of pointers to approximated curve points.
        -> CDouble -- ^ epsilon
        -> CBool -- ^ is closed
        -> IO (Ptr (C CvCppException))

    approxPolyDP_deletePtrArray
        :: Ptr (Ptr (Ptr (C (Point 2 depth))))
           -- ^ Array of pointers to approximated curve points.
        -> IO ()

instance ApproxPolyDP Int32 where
    approxPolyDP_internal curveSize curvePtr approxSizePtr approxPtrPtr epsilon isClosed =
      [cvExcept|
        cv::_InputArray curve =
          cv::_InputArray( $(Point2i * curvePtr)
                         , $(int32_t curveSize)
                         );
        std::vector<cv::Point2i> approx;
        cv::approxPolyDP
          ( curve
          , approx
          , $(double epsilon)
          , $(bool isClosed)
          );

        *$(size_t * approxSizePtr) = approx.size();

        cv::Point2i * * * approxPtrPtr = $(Point2i * * * approxPtrPtr);
        cv::Point2i * * approxPtr = new cv::Point2i * [approx.size()];
        *approxPtrPtr = approxPtr;

        for (std::vector<cv::Point2i>::size_type i = 0; i < approx.size(); i++)
        {
            cv::Point2i & ptAddress = approx[i];
            cv::Point2i * newPt = new cv::Point2i(ptAddress.x, ptAddress.y);
            approxPtr[i] = newPt;
        }
      |]

    approxPolyDP_deletePtrArray approxPtrPtr =
        [CU.block| void { delete [] *$(Point2i * * * approxPtrPtr); } |]

instance ApproxPolyDP CFloat where
    approxPolyDP_internal curveSize curvePtr approxSizePtr approxPtrPtr epsilon isClosed =
      [cvExcept|
        cv::_InputArray curve =
          cv::_InputArray( $(Point2f * curvePtr)
                         , $(int32_t curveSize)
                         );
        std::vector<cv::Point2f> approx;
        cv::approxPolyDP
          ( curve
          , approx
          , $(double epsilon)
          , $(bool isClosed)
          );

        *$(size_t * approxSizePtr) = approx.size();

        cv::Point2f * * * approxPtrPtr = $(Point2f * * * approxPtrPtr);
        cv::Point2f * * approxPtr = new cv::Point2f * [approx.size()];
        *approxPtrPtr = approxPtr;

        for (std::vector<cv::Point2f>::size_type i = 0; i < approx.size(); i++)
        {
            cv::Point2f & ptAddress = approx[i];
            cv::Point2f * newPt = new cv::Point2f(ptAddress.x, ptAddress.y);
            approxPtr[i] = newPt;
        }
      |]

    approxPolyDP_deletePtrArray approxPtrPtr =
        [CU.block| void { delete [] *$(Point2f * * * approxPtrPtr); } |]

--------------------------------------------------------------------------------

{- | Calculates the up-right bounding rectangle of a point set.

The function calculates and returns the minimal up-right bounding
rectangle for the specified point set.
-}
boundingRect
    :: forall point2 depth m
     . ( IsPoint2 point2 depth
       , BoundingRect depth
       , MonadError CvException m
       )
    => V.Vector (point2 depth)
    -> m Rect2i
boundingRect points
    | V.null points = throwError $ CvException "boundingRect: empty vector"
    | otherwise = unsafeWrapException $ do
        result <- toRectIO $ HRect 0 0
        unsafeWithArrayPtr (V.map toPoint points) $ \pointsPtr ->
          withPtr result $ \resultPtr ->
          handleCvException (pure result) $
          boundingRect_internal
            (fromIntegral $ V.length points)
            pointsPtr
            resultPtr

-- | Internal class used to overload the 'boundingRect' depth.
class ( CSizeOf      (C'Point 2 depth)
      , PlacementNew (C'Point 2 depth)
      ) => BoundingRect depth where
    boundingRect_internal
        :: Int32 -- ^ Number of input points.
        -> Ptr (C (Point 2 depth)) -- ^ Input points array.
        -> Ptr (C Rect2i) -- ^ Output rectangle.
        -> IO (Ptr (C CvCppException))

instance BoundingRect Int32 where
    boundingRect_internal pointsSize pointsPtr resultPtr =
        [cvExcept|
          cv::_InputArray points =
            cv::_InputArray( $(Point2i * pointsPtr)
                           , $(int32_t pointsSize)
                           );
          *$(Rect2i * resultPtr) = cv::boundingRect(points);
        |]

instance BoundingRect CFloat where
    boundingRect_internal pointsSize pointsPtr resultPtr =
        [cvExcept|
          cv::_InputArray points =
            cv::_InputArray( $(Point2f * pointsPtr)
                           , $(int32_t pointsSize)
                           );
          *$(Rect2i * resultPtr) = cv::boundingRect(points);
        |]

--------------------------------------------------------------------------------

{- | Finds the convex hull of a point set.

Finds the convex hull of a 2D point set using the Sklansky's algorithm
that has \( O(n \log n) \) complexity in the current implementation.

Example:

@
handConvexHullImg
    :: forall (width    :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *  )
     . (Mat ('S ['S height, 'S width]) ('S channels) ('S depth) ~ Hand)
    => IO (Mat ('S ['S height, 'S width]) ('S channels) ('S depth))
handConvexHullImg = do
    handContour <- mkHandContour
    let handHull = exceptError $ convexHull handContour True
    pure $ exceptError $
      withMatM (Proxy :: Proxy [height, width])
               (Proxy :: Proxy channels)
               (Proxy :: Proxy depth)
               transparent $ \\imgM -> do
        -- Draw contour.
        polylines imgM (V.singleton handContour) True blue 2 LineType_AA 0
        -- Draw convex hull of contour.
        polylines imgM (V.singleton handHull) True red 2 LineType_AA 0
@

<<doc/generated/examples/handConvexHull.png handConvexHullImg>>
-}
convexHull
    :: forall point2 depth m
     . ( IsPoint2 point2 depth
       , ConvexHull depth
       , MonadError CvException m
       )
    => V.Vector (point2 depth)
       -- ^ Input 2D point set.
    -> Bool
       -- ^ Orientation flag. If it is true, the output convex hull is oriented
       -- clockwise. Otherwise, it is oriented counter-clockwise. The assumed
       -- coordinate system has its X axis pointing to the right, and its Y axis
       -- pointing upwards.
    -> m (V.Vector (Point 2 depth))
       -- ^ Output convex hull.
convexHull points clockwise
    | V.null points = pure V.empty
    | otherwise = unsafeWrapException $
        unsafeWithArrayPtr points' $ \(pointsPtr :: Ptr (C (Point 2 depth))) ->
        alloca $ \(hullPointsPtrPtr :: Ptr (Ptr (Ptr (C (Point 2 depth))))) ->
        alloca $ \(hullSizePtr :: Ptr C.CSize) ->
        handleCvException
          ( do hullSize <- fromIntegral <$> peek hullSizePtr
               hullPointsPtr :: Ptr (Ptr (C (Point 2 depth))) <- peek hullPointsPtrPtr
               hullPointsList :: [Ptr (C (Point 2 depth))] <- peekArray hullSize hullPointsPtr
               hullPointsVec <- V.fromList <$> mapM (fromPtr . pure) hullPointsList
               convexHull_deletePtrArray hullPointsPtrPtr
               pure hullPointsVec
          ) $
          convexHull_internal
            (fromIntegral $ V.length points)
            pointsPtr
            (fromIntegral (fromBool clockwise))
            hullPointsPtrPtr
            hullSizePtr
  where
    points' :: V.Vector (Point 2 depth)
    points' = V.map toPoint points

-- | Internal class used to overload 'convexHull' depth.
class ( FromPtr      (Point   2 depth)
      , CSizeOf      (C'Point 2 depth)
      , PlacementNew (C'Point 2 depth)
      ) => ConvexHull depth where
    convexHull_internal
        :: Int32 -- ^ Number of input points.
        -> Ptr (C (Point 2 depth)) -- ^ Input points array.
        -> CBool -- ^ Orientation flag.
        -> Ptr (Ptr (Ptr (C (Point 2 depth))))
           -- ^ Array of pointers to hull points.
        -> Ptr C.CSize -- ^ Size of convex hull.
        -> IO (Ptr (C CvCppException))

    convexHull_deletePtrArray
        :: Ptr (Ptr (Ptr (C (Point 2 depth))))
           -- ^ Array of pointers to hull points.
        -> IO ()

instance ConvexHull Int32 where
    convexHull_internal numPoints pointsPtr clockwise hullPointsPtrPtr hullSizePtr =
        [cvExcept|
          cv::_InputArray points =
            cv::_InputArray( $(Point2i * pointsPtr)
                           , $(int32_t numPoints)
                           );
          std::vector<cv::Point2i> hull;
          cv::convexHull
            ( points
            , hull
            , $(bool clockwise)
            , true
            );

          *$(size_t * hullSizePtr) = hull.size();

          cv::Point2i * * * hullPointsPtrPtr = $(Point2i * * * hullPointsPtrPtr);
          cv::Point2i * * hullPointsPtr = new cv::Point2i * [hull.size()];
          *hullPointsPtrPtr = hullPointsPtr;

          for (std::vector<cv::Point2i>::size_type i = 0; i < hull.size(); i++)
          {
            cv::Point2i & hullPt = hull[i];
            cv::Point2i * newHullPt = new cv::Point2i(hullPt.x, hullPt.y);
            hullPointsPtr[i] = newHullPt;
          }
        |]

    convexHull_deletePtrArray hullPointsPtrPtr =
        [CU.block| void { delete [] *$(Point2i * * * hullPointsPtrPtr); }|]

instance ConvexHull CFloat where
    convexHull_internal numPoints pointsPtr clockwise hullPointsPtrPtr hullSizePtr =
        [cvExcept|
          cv::_InputArray points =
            cv::_InputArray( $(Point2f * pointsPtr)
                           , $(int32_t numPoints)
                           );
          std::vector<cv::Point2f> hull;
          cv::convexHull(points, hull, $(bool clockwise), true);

          *$(size_t * hullSizePtr) = hull.size();

          cv::Point2f * * * hullPointsPtrPtr = $(Point2f * * * hullPointsPtrPtr);
          cv::Point2f * * hullPointsPtr = new cv::Point2f * [hull.size()];
          *hullPointsPtrPtr = hullPointsPtr;

          for (std::vector<cv::Point2i>::size_type i = 0; i < hull.size(); i++)
          {
            cv::Point2f & hullPt = hull[i];
            cv::Point2f * newHullPt = new cv::Point2f(hullPt.x, hullPt.y);
            hullPointsPtr[i] = newHullPt;
          }
        |]

    convexHull_deletePtrArray hullPointsPtrPtr =
        [CU.block| void { delete [] *$(Point2f * * * hullPointsPtrPtr); }|]

--------------------------------------------------------------------------------

{- | Finds the convex hull of a point set.

Finds the convex hull of a 2D point set using the Sklansky's algorithm
that has \( O(n \log n) \) complexity in the current implementation.
-}
convexHullIndices
    :: forall point2 depth m
     . ( IsPoint2 point2 depth
       , ConvexHullIndices depth
       , MonadError CvException m
       )
    => V.Vector (point2 depth)
       -- ^ Input 2D point set.
    -> Bool
       -- ^ Orientation flag. If it is true, the output convex hull is oriented
       -- clockwise. Otherwise, it is oriented counter-clockwise. The assumed
       -- coordinate system has its X axis pointing to the right, and its Y axis
       -- pointing upwards.
    -> m (VS.Vector Int32)
       -- ^ Indices of those points in the input set that are part of the convex hull.
convexHullIndices points clockwise
    | V.null points = pure VS.empty
    | otherwise = unsafeWrapException $
        unsafeWithArrayPtr points' $ \(pointsPtr :: Ptr (C (Point 2 depth))) ->
        alloca $ \(hullIndicesPtrPtr :: Ptr (Ptr Int32)) ->
        alloca $ \(hullSizePtr :: Ptr C.CSize) ->
        handleCvException
          ( do hullSize <- fromIntegral <$> peek hullSizePtr
               -- The hullIndicesPtr points to memory allocated on the C++
               -- heap. We are responsible for eventually freeing this memory.
               hullIndicesPtr :: Ptr Int32 <- peek hullIndicesPtrPtr
               let freeHullIndices =
                     [CU.block| void { delete [] $(int32_t * hullIndicesPtr); }|]
               hullIndicesForeignPtr <-
                 newForeignPtr hullIndicesPtr freeHullIndices
               pure $ VS.unsafeFromForeignPtr0 hullIndicesForeignPtr hullSize
          ) $
          convexHullIndices_internal
            (fromIntegral $ V.length points)
            pointsPtr
            (fromIntegral (fromBool clockwise))
            hullIndicesPtrPtr
            hullSizePtr
  where
    points' :: V.Vector (Point 2 depth)
    points' = V.map toPoint points

-- | Internal class used to overload 'convexHullIndices' depth.
class ( FromPtr      (Point   2 depth)
      , CSizeOf      (C'Point 2 depth)
      , PlacementNew (C'Point 2 depth)
      ) => ConvexHullIndices depth where
    convexHullIndices_internal
        :: Int32 -- ^ Number of input points.
        -> Ptr (C (Point 2 depth)) -- ^ Input points array.
        -> CBool -- ^ Orientation flag.
        -> Ptr (Ptr Int32) -- ^ Array of convex hull indices.
        -> Ptr C.CSize -- ^ Size of convex hull indices.
        -> IO (Ptr (C CvCppException))

instance ConvexHullIndices Int32 where
    convexHullIndices_internal numPoints pointsPtr clockwise hullIndicesPtrPtr hullSizePtr =
        [cvExcept|
          cv::_InputArray points =
            cv::_InputArray( $(Point2i * pointsPtr)
                           , $(int32_t numPoints)
                           );
          std::vector<int32_t> hullIndices;

          cv::convexHull(points, hullIndices, $(bool  clockwise), false);

          *$(size_t * hullSizePtr) = hullIndices.size();
          int32_t * hullIndicesArr = new int32_t [hullIndices.size()];
          std::copy(hullIndices.begin(), hullIndices.end(), hullIndicesArr);
          *$(int32_t * * hullIndicesPtrPtr) = hullIndicesArr;
        |]

instance ConvexHullIndices CFloat where
    convexHullIndices_internal numPoints pointsPtr clockwise hullIndicesPtrPtr hullSizePtr =
        [cvExcept|
          cv::_InputArray points =
            cv::_InputArray( $(Point2f * pointsPtr)
                           , $(int32_t numPoints)
                           );
          std::vector<int32_t> hullIndices;

          cv::convexHull(points, hullIndices, $(bool clockwise), false);

          *$(size_t * hullSizePtr) = hullIndices.size();
          int32_t * hullIndicesArr = new int32_t [hullIndices.size()];
          std::copy(hullIndices.begin(), hullIndices.end(), hullIndicesArr);
          *$(int32_t * * hullIndicesPtrPtr) = hullIndicesArr;
        |]
