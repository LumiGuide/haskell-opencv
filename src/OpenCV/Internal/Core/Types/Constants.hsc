{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Constants where

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "termcriteria.hpp"

#include "namespace.hpp"
#include "hsc_macros.hpp"

#sizeof Point2i
#sizeof Point2f
#sizeof Point2d

#sizeof Point3i
#sizeof Point3f
#sizeof Point3d

#sizeof Size2i
#sizeof Size2f

#sizeof Scalar
#sizeof Range
#sizeof KeyPoint
#sizeof DMatch

#sizeof Mat

#num TERMCRITERIA_COUNT
#num TERMCRITERIA_EPS
