{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module OpenCV.Core.Types.Matx
    ( -- * Abstract Matx
      Matx
    , MatxDimR
    , MatxDimC
    , IsMatx(..)

      -- * Matx's of specific sizes
    , Matx12f, Matx12d
    , Matx13f, Matx13d
    , Matx14f, Matx14d
    , Matx16f, Matx16d
    , Matx21f, Matx21d
    , Matx22f, Matx22d
    , Matx23f, Matx23d
    , Matx31f, Matx31d
    , Matx32f, Matx32d
    , Matx33f, Matx33d
    , Matx34f, Matx34d
    , Matx41f, Matx41d
    , Matx43f, Matx43d
    , Matx44f, Matx44d
    , Matx51f, Matx51d
    , Matx61f, Matx61d
    , Matx66f, Matx66d

      -- * Constructors
    , newMatx12f, newMatx12d
    , newMatx13f, newMatx13d
    , newMatx14f, newMatx14d
    , newMatx16f, newMatx16d
    , newMatx21f, newMatx21d
    , newMatx22f, newMatx22d
    , newMatx23f, newMatx23d
    , newMatx31f, newMatx31d
    , newMatx32f, newMatx32d
    , newMatx33f, newMatx33d
    , newMatx34f, newMatx34d
    , newMatx41f, newMatx41d
    , newMatx43f, newMatx43d
    , newMatx44f, newMatx44d
    , newMatx51f, newMatx51d
    , newMatx61f, newMatx61d
    ) where

import "base" Foreign.C.Types
import qualified "inline-c"     Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C ( using )
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Matx
import "this" OpenCV.Internal.Core.Types.Matx.TH

--------------------------------------------------------------------------------

C.context openCvCtx
C.include "opencv2/core.hpp"
C.include "haskell_opencv_matx_typedefs.hpp"
C.using "namespace cv"

mkMatxType "Matx12f" 1 2 ''CFloat  ''C'Matx12f "float"
mkMatxType "Matx12d" 1 2 ''CDouble ''C'Matx12d "double"
mkMatxType "Matx13f" 1 3 ''CFloat  ''C'Matx13f "float"
mkMatxType "Matx13d" 1 3 ''CDouble ''C'Matx13d "double"
mkMatxType "Matx14f" 1 4 ''CFloat  ''C'Matx14f "float"
mkMatxType "Matx14d" 1 4 ''CDouble ''C'Matx14d "double"
mkMatxType "Matx16f" 1 6 ''CFloat  ''C'Matx16f "float"
mkMatxType "Matx16d" 1 6 ''CDouble ''C'Matx16d "double"
mkMatxType "Matx21f" 2 1 ''CFloat  ''C'Matx21f "float"
mkMatxType "Matx21d" 2 1 ''CDouble ''C'Matx21d "double"
mkMatxType "Matx22f" 2 2 ''CFloat  ''C'Matx22f "float"
mkMatxType "Matx22d" 2 2 ''CDouble ''C'Matx22d "double"
mkMatxType "Matx23f" 2 3 ''CFloat  ''C'Matx23f "float"
mkMatxType "Matx23d" 2 3 ''CDouble ''C'Matx23d "double"
mkMatxType "Matx31f" 3 1 ''CFloat  ''C'Matx31f "float"
mkMatxType "Matx31d" 3 1 ''CDouble ''C'Matx31d "double"
mkMatxType "Matx32f" 3 2 ''CFloat  ''C'Matx32f "float"
mkMatxType "Matx32d" 3 2 ''CDouble ''C'Matx32d "double"
mkMatxType "Matx33f" 3 3 ''CFloat  ''C'Matx33f "float"
mkMatxType "Matx33d" 3 3 ''CDouble ''C'Matx33d "double"
mkMatxType "Matx34f" 3 4 ''CFloat  ''C'Matx34f "float"
mkMatxType "Matx34d" 3 4 ''CDouble ''C'Matx34d "double"
mkMatxType "Matx41f" 4 1 ''CFloat  ''C'Matx41f "float"
mkMatxType "Matx41d" 4 1 ''CDouble ''C'Matx41d "double"
mkMatxType "Matx43f" 4 3 ''CFloat  ''C'Matx43f "float"
mkMatxType "Matx43d" 4 3 ''CDouble ''C'Matx43d "double"
mkMatxType "Matx44f" 4 4 ''CFloat  ''C'Matx44f "float"
mkMatxType "Matx44d" 4 4 ''CDouble ''C'Matx44d "double"
mkMatxType "Matx51f" 5 1 ''CFloat  ''C'Matx51f "float"
mkMatxType "Matx51d" 5 1 ''CDouble ''C'Matx51d "double"
mkMatxType "Matx61f" 6 1 ''CFloat  ''C'Matx61f "float"
mkMatxType "Matx61d" 6 1 ''CDouble ''C'Matx61d "double"
mkMatxType "Matx66f" 6 6 ''CFloat  ''C'Matx66f "float"
mkMatxType "Matx66d" 6 6 ''CDouble ''C'Matx66d "double"
