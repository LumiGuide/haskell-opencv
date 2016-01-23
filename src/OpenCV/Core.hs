{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module OpenCV.Core
    ( -- *** Vectors of Vectors
      matToM23
    , matToM33
    ) where

import "base" Foreign.Storable ( Storable )
import "linear" Linear.Matrix ( M23, M33 )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import qualified "repa" Data.Array.Repa as Repa
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Core.Types.Mat.Repa
import "this" OpenCV.TypeLevel

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
    V2 (V3 (i 0 0) (i 1 0) (i 2 0))
       (V3 (i 0 1) (i 1 1) (i 2 1))
  where
    i row col = Repa.unsafeIndex a $ Repa.ix3 0 row col

repaToM33 :: (Storable e) => Repa.Array (M '[ 'S 3, 'S 3 ] 1) Repa.DIM3 e -> M33 e
repaToM33 a =
    V3 (V3 (i 0 0) (i 1 0) (i 2 0))
       (V3 (i 0 1) (i 1 1) (i 2 1))
       (V3 (i 0 2) (i 1 2) (i 2 2))
  where
    i row col = Repa.unsafeIndex a $ Repa.ix3 0 row col
