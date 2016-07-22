{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module OpenCV.C.PlacementNew.TH ( mkPlacementNewInstance ) where

import "base" Foreign.Ptr ( Ptr )
--import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Internal as C
import qualified "inline-c" Language.C.Types as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
--import "this" OpenCV.C.Types
import "this" OpenCV.C.Inline ( openCvCtx )
import "template-haskell" Language.Haskell.TH
--import "template-haskell" Language.Haskell.TH.Syntax
import "this" OpenCV.C.PlacementNew ( PlacementNew (..) )

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

mkPlacementNewInstance :: Name -> DecsQ
mkPlacementNewInstance name =
    [d|
      instance PlacementNew $(conT (mkName ctypeName)) where
          placementNew =
              $(
                C.inlineExp
                  Safe
                  [t| Ptr $(conT name) -> Ptr $(conT name) -> IO () |]
                  (C.quickCParser_ "void" C.parseType)
                  [ ("src", C.quickCParser_ (typeName ++ " *") C.parseType)
                  , ("dst", C.quickCParser_ (typeName ++ " *") C.parseType)
                  ]
                  ("new(dst) cv::" ++ typeName ++ "(*src)")
              )
          placementDelete =
              $(
                C.inlineExp
                  Safe
                  [t| Ptr $(conT name) -> IO () |]
                  (C.quickCParser_ "void" C.parseType)
                  [ ("ptr", C.quickCParser_ (typeName ++ " *") C.parseType)
                  ]
                  ("ptr->~" ++ typeName ++ "()")
              )
    |]
  where
    typeName = nameBase name
    ctypeName = 'C' : typeName
