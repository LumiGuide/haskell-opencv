{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Rect.TH
  ( mkRectType
  ) where

import "base" Data.Monoid ( (<>) )
import "base" Foreign.Marshal.Utils ( toBool )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import "linear" Linear.Vector ( (^+^) )
import "linear" Linear.V2 ( V2(..) )
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Quote ( quoteExp )
import "this" OpenCV.Core.Types.Point
import "this" OpenCV.Core.Types.Size
import "this" OpenCV.Internal
import "this" OpenCV.Internal.Core.Types.Rect
import "this" OpenCV.Internal.C.PlacementNew.TH ( mkPlacementNewInstance )
import "this" OpenCV.Internal.C.Types

--------------------------------------------------------------------------------

mkRectType
    :: String -- ^ Rectangle type name, for both Haskell and C
    -> Name   -- ^ Depth type name in Haskell
    -> String -- ^ Depth type name in C
    -> String -- ^ Point type name in C
    -> String -- ^ Size type name in C
    -> Q [Dec]
mkRectType rTypeNameStr depthTypeName cDepthTypeStr cPointTypeStr cSizeTypeStr =
    fmap concat . sequence $
      [ (:[]) <$> rectTySynD
      , fromPtrDs
      , isRectOpenCVInstanceDs
      , isRectHaskellInstanceDs
      , mkPlacementNewInstance rTypeName
      ]
  where
    rTypeName :: Name
    rTypeName = mkName rTypeNameStr

    cRectTypeStr :: String
    cRectTypeStr = rTypeNameStr

    rTypeQ :: Q Type
    rTypeQ = conT rTypeName

    depthTypeQ :: Q Type
    depthTypeQ = conT depthTypeName

    rectTySynD :: Q Dec
    rectTySynD =
        tySynD rTypeName [] ([t|Rect|] `appT` depthTypeQ)

    fromPtrDs :: Q [Dec]
    fromPtrDs =
        [d|
        instance FromPtr $(rTypeQ) where
          fromPtr = objFromPtr Rect $ $(finalizerExpQ)
        |]
      where
        finalizerExpQ :: Q Exp
        finalizerExpQ = do
          ptr <- newName "ptr"
          lamE [varP ptr] $
            quoteExp CU.exp $
              "void { delete $(" <> cRectTypeStr <> " * " <> nameBase ptr <> ") }"

    isRectOpenCVInstanceDs :: Q [Dec]
    isRectOpenCVInstanceDs =
        [d|
        instance IsRect Rect $(depthTypeQ) where
          toRect   = id
          fromRect = id

          rectTopLeft     rect = unsafePerformIO $ fromPtr $ withPtr rect $ $(rectTopLeftExpQ)
          rectBottomRight rect = unsafePerformIO $ fromPtr $ withPtr rect $ $(rectBottomRightExpQ)
          rectSize        rect = unsafePerformIO $ fromPtr $ withPtr rect $ $(rectSizeExpQ)
          rectArea        rect = unsafePerformIO $ withPtr rect $ $(rectAreaExpQ)
          rectContains         = $(rectContainsExpQ)
        |]
      where
        rectTopLeftExpQ :: Q Exp
        rectTopLeftExpQ = do
          rectPtr <- newName "rectPtr"
          lamE [varP rectPtr] $ quoteExp CU.exp $
            cPointTypeStr <> " * { new " <> cPointTypeStr <> "($(" <> cRectTypeStr <> " * rectPtr)->tl()) }"

        rectBottomRightExpQ :: Q Exp
        rectBottomRightExpQ = do
          rectPtr <- newName "rectPtr"
          lamE [varP rectPtr] $ quoteExp CU.exp $
            cPointTypeStr <> " * { new " <> cPointTypeStr <> "($(" <> cRectTypeStr <> " * rectPtr)->br()) }"

        rectSizeExpQ :: Q Exp
        rectSizeExpQ = do
          rectPtr <- newName "rectPtr"
          lamE [varP rectPtr] $ quoteExp CU.exp $
            cSizeTypeStr <> " * { new " <> cSizeTypeStr <> "($(" <> cRectTypeStr <> " * rectPtr)->size()) }"

        rectAreaExpQ :: Q Exp
        rectAreaExpQ = do
          rectPtr <- newName "rectPtr"
          lamE [varP rectPtr] $ quoteExp CU.exp $
            cDepthTypeStr <> " { $(" <> cRectTypeStr <> " * rectPtr)->area() }"

        rectContainsExpQ :: Q Exp
        rectContainsExpQ = do
          point <- newName "point"
          rect  <- newName "rect"
          pointPtr <- newName "pointPtr"
          rectPtr  <- newName "rectPtr"

          lamE [varP point, varP rect]
            $ appE [e|toBool|]
            $ appE [e|unsafePerformIO|]
            $ appE ([e|withPtr|] `appE` ([e|toPoint|] `appE` varE point))
            $ lamE [varP pointPtr]
            $ appE ([e|withPtr|] `appE` (varE rect))
            $ lamE [varP rectPtr]
            $ quoteExp CU.exp
            $ "int { $(" <> cRectTypeStr <> " * rectPtr)->contains(*$(" <> cPointTypeStr <> " * pointPtr)) }"

    isRectHaskellInstanceDs :: Q [Dec]
    isRectHaskellInstanceDs =
        [d|
        instance IsRect HRect $(depthTypeQ) where
          toRect hr = unsafePerformIO $ toRectIO hr
          fromRect rect = HRect
                          { hRectTopLeft = fromPoint $ rectTopLeft rect
                          , hRectSize    = fromSize  $ rectSize    rect
                          }

          toRectIO = $(toRectIOExpQ)

          rectTopLeft     hr = hRectTopLeft hr
          rectBottomRight hr = hRectTopLeft hr ^+^ hRectSize hr
          rectSize        hr = hRectSize hr

          rectArea hr = let V2 w h = hRectSize hr
                        in w * h

          rectContains (V2 px py) (HRect (V2 rx ry) (V2 rw rh)) =
                 px >= rx && px < rx + rw
              && py >= ry && py < ry + rh
        |]
      where
        toRectIOExpQ :: Q Exp
        toRectIOExpQ = do
          x <- newName "x"
          y <- newName "y"
          w <- newName "w"
          h <- newName "h"
          lamE [conP 'HRect [conP 'V2 [varP x, varP y], conP 'V2 [varP w, varP h]]] $
            appE [e|fromPtr|] $
              quoteExp CU.exp $ concat
                [ cRectTypeStr <> " * { "
                , "new cv::Rect_<" <> cDepthTypeStr <> ">("
                , "$(" <> cDepthTypeStr <> " x), "
                , "$(" <> cDepthTypeStr <> " y), "
                , "$(" <> cDepthTypeStr <> " w), "
                , "$(" <> cDepthTypeStr <> " h)"
                , ")}"
                ]
