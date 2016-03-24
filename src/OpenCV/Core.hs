{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Core
    ( -- *** Vectors of Vectors
      matToM23
    , matToM33
    , m23ToMat
    , m33ToMat
    ) where

import "base" Data.Proxy ( Proxy(..) )
import "base" Foreign.Storable ( Storable )
import "linear" Linear.Matrix ( M23, M33 )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4 )
import qualified "repa" Data.Array.Repa as Repa
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Core.Types.Mat.Repa
import "this" OpenCV.TypeLevel
import "this" OpenCV.Unsafe

--------------------------------------------------------------------------------

matToM23
    :: (Storable depth)
    => Mat (ShapeT [2, 3]) ('S 1) ('S depth) -- ^
    -> M23 depth
matToM23 = repaToM23 . toRepa

matToM33
    :: (Storable depth)
    => Mat (ShapeT [3, 3]) ('S 1) ('S depth) -- ^
    -> M33 depth
matToM33 = repaToM33 . toRepa

repaToM23 :: (Storable e) => Repa.Array (M '[ 'S 2, 'S 3 ] 1) Repa.DIM3 e -> M23 e
repaToM23 a =
    V2 (V3 (i 0 0) (i 0 1) (i 0 2))
       (V3 (i 1 0) (i 1 1) (i 1 2))
  where
    i row col = Repa.unsafeIndex a $ Repa.ix3 0 col row

repaToM33 :: (Storable e) => Repa.Array (M '[ 'S 3, 'S 3 ] 1) Repa.DIM3 e -> M33 e
repaToM33 a =
    V3 (V3 (i 0 0) (i 0 1) (i 0 2))
       (V3 (i 1 0) (i 1 1) (i 1 2))
       (V3 (i 2 0) (i 2 1) (i 2 2))
  where
    i row col = Repa.unsafeIndex a $ Repa.ix3 0 col row

m23ToMat :: forall depth
          . ( Convert (Proxy depth) Depth
            , Storable depth
            )
         => M23 depth
         -> Mat (ShapeT [2, 3]) ('S 1) ('S depth) -- ^
m23ToMat (V2 (V3 i00 i01 i02)
             (V3 i10 i11 i12)
         )
    = createMat $ do
    imgM <- mkMatM (Proxy :: Proxy [2, 3])
                   (Proxy :: Proxy 1)
                   (Proxy :: Proxy depth)
                   (pure 0 :: V4 Double)
    unsafeWrite imgM [0, 0] i00
    unsafeWrite imgM [1, 0] i10
    unsafeWrite imgM [0, 1] i01
    unsafeWrite imgM [1, 1] i11
    unsafeWrite imgM [0, 2] i02
    unsafeWrite imgM [1, 2] i12
    pure imgM

m33ToMat :: forall depth
          . ( Convert (Proxy depth) Depth
            , Storable depth
            )
         => M33 depth
         -> Mat (ShapeT [3, 3]) ('S 1) ('S depth) -- ^
m33ToMat (V3 (V3 i00 i01 i02)
             (V3 i10 i11 i12)
             (V3 i20 i21 i22)
         )
    = createMat $ do
    imgM <- mkMatM (Proxy :: Proxy [3, 3])
                   (Proxy :: Proxy 1)
                   (Proxy :: Proxy depth)
                   (pure 0 :: V4 Double)
    unsafeWrite imgM [0, 0] i00
    unsafeWrite imgM [1, 0] i10
    unsafeWrite imgM [2, 0] i20
    unsafeWrite imgM [0, 1] i01
    unsafeWrite imgM [1, 1] i11
    unsafeWrite imgM [2, 1] i21
    unsafeWrite imgM [0, 2] i02
    unsafeWrite imgM [1, 2] i12
    unsafeWrite imgM [2, 2] i22
    pure imgM
