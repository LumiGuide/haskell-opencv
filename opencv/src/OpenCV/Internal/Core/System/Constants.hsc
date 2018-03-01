{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.System.Constants where

#include <bindings.dsl.h>
#include "opencv2/core.hpp"

#num CV_CPU_MMX
#num CV_CPU_SSE
#num CV_CPU_SSE2
#num CV_CPU_SSE3
#num CV_CPU_SSSE3
#num CV_CPU_SSE4_1
#num CV_CPU_SSE4_2
#num CV_CPU_POPCNT
#num CV_CPU_AVX
#num CV_CPU_AVX2
