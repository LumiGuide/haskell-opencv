{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module OpenCV.C.PlacementNew.TH ( mkPlacementNewInstance ) where

import "base" Data.Monoid ( (<>) )
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Quote ( quoteExp )
import "this" OpenCV.C.PlacementNew ( PlacementNew (..) )

mkPlacementNewInstance :: Name -> DecsQ
mkPlacementNewInstance name =
    [d|
      instance PlacementNew $(conT (mkName ctypeName)) where
          placementNew    = $(placementNewQ)
          placementDelete = $(placementDeleteQ)
    |]
  where
    typeName = nameBase name
    ctypeName = "C'"  <> typeName

    placementNewQ = do
      dst <- newName "dst"
      src <- newName "src"
      lamE [varP dst, varP src] $
        quoteExp C.exp $
          "void { new($(" <> typeName <> " * " <> nameBase dst <> ")) cv::" <> typeName <>
                   "(*$(" <> typeName <> " * " <> nameBase src <> ")) }"

    placementDeleteQ = do
      ptr <- newName "ptr"
      lamE [varP ptr] $
        quoteExp C.exp $
          "void { $(" <> typeName <> " * " <> nameBase ptr <> ")->~" <> typeName <> "() }"
