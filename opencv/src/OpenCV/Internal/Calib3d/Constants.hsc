{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Calib3d.Constants where

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/calib3d.hpp"

#include "namespace.hpp"

#ifdef OPENCV4
#num FM_7POINT
#num FM_8POINT
#num FM_RANSAC
#num FM_LMEDS
#else
#num CV_FM_7POINT
#num CV_FM_8POINT
#num CV_FM_RANSAC
#num CV_FM_LMEDS
c'FM_7POINT :: Num a => a
c'FM_8POINT :: Num a => a
c'FM_RANSAC :: Num a => a
c'FM_LMEDS  :: Num a => a
c'FM_7POINT = c'CV_FM_7POINT
c'FM_8POINT = c'CV_FM_8POINT
c'FM_RANSAC = c'CV_FM_RANSAC
c'FM_LMEDS  = c'CV_FM_LMEDS
#endif

#num LMEDS
#num RANSAC
#num RHO

#num SOLVEPNP_ITERATIVE
#num SOLVEPNP_P3P
#num SOLVEPNP_AP3P
#num SOLVEPNP_EPNP
#num SOLVEPNP_DLS
#num SOLVEPNP_UPNP
