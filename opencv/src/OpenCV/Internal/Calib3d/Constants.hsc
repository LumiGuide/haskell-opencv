{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Calib3d.Constants where

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/calib3d.hpp"

#include "namespace.hpp"

#num FM_7POINT
#num FM_8POINT
#num FM_RANSAC
#num FM_LMEDS

#num LMEDS
#num RANSAC
#num RHO

#num SOLVEPNP_ITERATIVE
#num SOLVEPNP_P3P
#num SOLVEPNP_AP3P
#num SOLVEPNP_EPNP
#num SOLVEPNP_DLS
#num SOLVEPNP_UPNP
