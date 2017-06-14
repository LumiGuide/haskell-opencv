{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#if __GLASGOW_HASKELL__ >= 800
{-# options_ghc -Wno-redundant-constraints #-}
#endif

module OpenCV.Core.Cluster
  ( KmeansFlags
  , kmeans
  ) where

import "base" Data.Int
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Exception
import "this" OpenCV.TypeLevel
import "transformers" Control.Monad.Trans.Except

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"

#include "namespace.hpp"

--------------------------------------------------------------------------------

data KmeansFlags
   = Kmeans_RandomCenters
     -- ^ Select random initial centers in each attempt.
   | Kmeans_PpCenters
     -- ^ Use kmeans++ center initialization by Arthur and
     -- Vassilvitskii [Arthur2007].
   | Kmeans_UseInitialLabels
     -- ^ During the first (and possibly the only) attempt, use the
     -- user-supplied labels instead of computing them from the
     -- initial centers. For the second and further attempts, use the
     -- random or semi-random centers. Use one of
     -- 'Kmeans_RandomCenters' or 'Kmeans_PpCenters' flag to specify
     -- the exact method.
     deriving (Show, Enum, Bounded)

#num KMEANS_RANDOM_CENTERS
#num KMEANS_PP_CENTERS
#num KMEANS_USE_INITIAL_LABELS

marshalKmeansFlags :: KmeansFlags -> Int32
marshalKmeansFlags = \case
    Kmeans_RandomCenters    -> c'KMEANS_RANDOM_CENTERS
    Kmeans_PpCenters        -> c'KMEANS_PP_CENTERS
    Kmeans_UseInitialLabels -> c'KMEANS_USE_INITIAL_LABELS

kmeans
    :: forall numClusters numPoints pointDim m
     . ( ToInt32 numClusters
       , PrimMonad m
       )
    => Mat ('S '[numPoints, 'S 1]) ('S pointDim) ('S Float)
       -- ^ data: Data for clustering.
    -> numClusters
       -- ^ K: Number of clusters to split the set by.
    -> Mut (Mat ('S '[numPoints, 'S 1]) ('S 1) ('S Int32)) (PrimState m)
       -- ^ bestLabels: Input/output integer array that stores the
       -- cluster indices for every sample.
    -> TermCriteria
       -- ^ criteria: The algorithm termination criteria, that is, the
       -- maximum number of iterations and/or the desired
       -- accuracy. The accuracy is specified as @epsilon@. As soon as
       -- each of the cluster centers moves by less than @epsilon@ on
       -- some iteration, the algorithm stops.
    -> Int
       -- ^ attempts: Flag to specify the number of times the
       -- algorithm is executed using different initial labelings. The
       -- algorithm returns the labels that yield the best compactness
       -- (see the 'KmeansFlags').
    -> KmeansFlags
    -> CvExceptT m (Double, Mat ('S '[DSNat numClusters, 'S 1]) ('S 2) ('S Float))
       -- ^ (compactness, centers)
       --
       -- compactness: The function returns the compactness
       -- measure. The best (minimum) value is chosen and the
       -- corresponding labels and the compactness value are returned
       -- by the function. Basically, you can use only the core of the
       -- function, set the number of attempts to 1, initialize labels
       -- each time using a custom algorithm, pass them with the (
       -- flags = KMEANS_USE_INITIAL_LABELS ) flag, and then choose
       -- the best (most-compact) clustering.
       --
       -- centers: Output matrix of the cluster centers, one row per
       -- cluster center.
kmeans clusterData numClusters bestLabels criteria attempts flags =
    ExceptT $ unsafePrimToPrim $ do
      centers <- unsafeCoerceMat <$> newEmptyMat
      alloca $ \compactnessPtr ->
        handleCvException ((, centers) <$> (realToFrac <$> peek compactnessPtr)) $
          withPtr clusterData $ \clusterDataPtr ->
          withPtr bestLabels  $ \bestLabelsPtr  ->
          withPtr centers     $ \centersPtr     ->
          withPtr criteria    $ \criteriaPtr    ->
            [cvExcept|
              *$(double * compactnessPtr) =
                cv::kmeans
                  ( *$(Mat * clusterDataPtr)
                  , $(int32_t c'numClusters)
                  , *$(Mat * bestLabelsPtr)
                  , *$(TermCriteria * criteriaPtr)
                  , $(int32_t c'attempts)
                  , $(int32_t c'flags)
                  , *$(Mat * centersPtr)
                  );
            |]
  where
    c'numClusters = toInt32 numClusters
    c'attempts = fromIntegral attempts :: Int32
    c'flags = marshalKmeansFlags flags
