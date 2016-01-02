{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module OpenCV.Core
    ( -- *** Vectors of Vectors
      matToM23
    , matToM33
    ) where

import "linear" Linear.Matrix ( M23, M33 )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "lumi-hackage-extended" Lumi.Prelude
import qualified "repa" Data.Array.Repa as Repa
import "repa" Data.Array.Repa.Index ( Z(Z), (:.)((:.)) )
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Core.Types.Mat.Repa

--------------------------------------------------------------------------------

matToM23 :: (MatElem e) => Mat -> Either String (M23 e)
matToM23 = fmap repaToM23 . toRepa

matToM33 :: (MatElem e) => Mat -> Either String (M33 e)
matToM33 = fmap repaToM33 . toRepa

repaToM23 :: (Storable e) => Repa.Array M Repa.DIM2 e -> M23 e
repaToM23 a =
    V2 (V3 (i 0 0) (i 1 0) (i 2 0))
       (V3 (i 0 1) (i 1 1) (i 2 1))
  where
    i row col = Repa.unsafeIndex a $ Z :. row :. col

repaToM33 :: (Storable e) => Repa.Array M Repa.DIM2 e -> M33 e
repaToM33 a =
    V3 (V3 (i 0 0) (i 1 0) (i 2 0))
       (V3 (i 0 1) (i 1 1) (i 2 1))
       (V3 (i 0 2) (i 1 2) (i 2 2))
  where
    i row col = Repa.unsafeIndex a $ Z :. row :. col
