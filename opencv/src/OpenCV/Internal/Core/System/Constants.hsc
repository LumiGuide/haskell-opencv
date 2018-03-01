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
#num CV_CPU_FP16
#num CV_CPU_AVX
#num CV_CPU_AVX2
#num CV_CPU_FMA3
#num CV_CPU_AVX_512F
#num CV_CPU_AVX_512BW
#num CV_CPU_AVX_512CD
#num CV_CPU_AVX_512DQ
#num CV_CPU_AVX_512ER
-- #num CV_CPU_AVX_512IFMA512
-- #num CV_CPU_AVX_512IFMA
#num CV_CPU_AVX_512PF
#num CV_CPU_AVX_512VBMI
#num CV_CPU_AVX_512VL
#num CV_CPU_NEON
#num CV_CPU_VSX
-- #num CV_CPU_AVX512_SKX
