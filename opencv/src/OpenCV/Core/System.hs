{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module OpenCV.Core.System
    ( getBuildInformation
    , HardwareFeature(..)
    , checkHardwareSupport
    , getNumberOfCPUs
    , getNumThreads
    , getThreadNum
    , getTickCount
    , getTickFrequency
    , getCPUTickCount
    , setNumThreads
    , setUseOptimized
    , useOptimized
    ) where

import "base" Data.Int
import "base" Foreign.C.String ( peekCString )
import "base" Foreign.Marshal.Utils ( toBool, fromBool )
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types ( fromCDouble )
import "this" OpenCV.Internal.Core.System.Constants

 --------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

-- | Returns full configuration time cmake output.
--
-- Returned value is raw cmake output including version control system revision,
-- compiler version, compiler flags, enabled modules and third party libraries,
-- etc. Output format depends on target architecture.
getBuildInformation :: IO String
getBuildInformation =
    peekCString =<< [C.exp| const char * { cv::getBuildInformation().c_str() }|]

-- | Features which may are may not be supported by the host hardware.
--
-- Use 'checkHardwareSupport' to query which features are available.
data HardwareFeature
   = MMX      -- ^ MMX
   | SSE      -- ^ SSE
   | SSE2     -- ^ SSE 2
   | SSE3     -- ^ SSE 3
   | SSSE3    -- ^ SSSE 3
   | SSE4_1   -- ^ SSE 4.1
   | SSE4_2   -- ^ SSE 4.2
   | POPCOUNT -- ^ POPCOUNT
   | AVX      -- ^ AVX
   | AVX2     -- ^ AVX2

marshalHardwareFeature :: HardwareFeature -> Int32
marshalHardwareFeature = \case
    MMX      -> c'CV_CPU_MMX
    SSE      -> c'CV_CPU_SSE
    SSE2     -> c'CV_CPU_SSE2
    SSE3     -> c'CV_CPU_SSE3
    SSSE3    -> c'CV_CPU_SSSE3
    SSE4_1   -> c'CV_CPU_SSE4_1
    SSE4_2   -> c'CV_CPU_SSE4_2
    POPCOUNT -> c'CV_CPU_POPCNT
    AVX      -> c'CV_CPU_AVX
    AVX2     -> c'CV_CPU_AVX2


-- | Returns @True@ if the specified feature is supported by the host hardware.
--
-- The function returns @True@ if the host hardware supports the specified
-- feature. When user calls @'setUseOptimized' False@, the subsequent calls to
-- 'checkHardwareSupport' will return @False@ until @'setUseOptimized' True@ is
-- called. This way user can dynamically switch on and off the optimized code in
-- OpenCV.
checkHardwareSupport :: HardwareFeature -> IO Bool
checkHardwareSupport hardwareFeature = toBool <$>
    [C.exp| bool { cv::checkHardwareSupport($(int32_t c'hardwareFeature)) }|]
  where
    c'hardwareFeature = marshalHardwareFeature hardwareFeature

-- | Returns the number of logical CPUs available for the process.
getNumberOfCPUs :: IO Int32
getNumberOfCPUs = [C.exp| int32_t { cv::getNumberOfCPUs() }|]

{- | Returns the number of threads used by OpenCV for parallel regions. Always
returns 1 if OpenCV is built without threading support.

The exact meaning of return value depends on the threading framework used by
OpenCV library:

  * __TBB__ - The number of threads, that OpenCV will try to use for parallel
    regions. If there is any @tbb::thread_scheduler_init@ in user code
    conflicting with OpenCV, then function returns default number of threads
    used by TBB library.

  * __OpenMP__ - An upper bound on the number of threads that could be used to
    form a new team.

  * __Concurrency__ - The number of threads, that OpenCV will try to use for
    parallel regions.

  * __GCD__ - Unsupported; returns the GCD thread pool limit (512) for
    compatibility.

  * __C=__ - The number of threads, that OpenCV will try to use for parallel
    regions, if before called 'setNumThreads' with threads > 0, otherwise
    returns the number of logical CPUs, available for the process.
-}
getNumThreads :: IO Int32
getNumThreads = [C.exp| int32_t { cv::getNumThreads() }|]

{- | Returns the index of the currently executed thread within the current parallel region.

Always returns 0 if called outside of parallel region.

The exact meaning of the return value depends on the threading framework used by OpenCV library:

  * __TBB__ - Unsupported with current 4.1 TBB release. May be will be supported
    in future.

  * __OpenMP__ - The thread number, within the current team, of the calling
    thread.

  * __Concurrency__ - An ID for the virtual processor that the current context
    is executing on (0 for master thread and unique number for others, but not
    necessary 1,2,3,...).

  * __GCD__ - System calling thread’s ID. Never returns 0 inside parallel
    region.

  * __C=__ - The index of the current parallel task.
-}
getThreadNum :: IO Int32
getThreadNum = [C.exp| int32_t { cv::getThreadNum() }|]

-- | Returns the number of ticks.
--
-- The function returns the number of ticks after the certain event (for
-- example, when the machine was turned on). It can be used to initialize RNG()
-- or to measure a function execution time by reading the tick count before and
-- after the function call. See 'getTickFrequency'.
getTickCount :: IO Int64
getTickCount = [C.exp| int64_t { cv::getTickCount() }|]

-- | Returns the number of ticks per second.
--
-- See 'getTickCount'.
getTickFrequency :: IO Double
getTickFrequency = fromCDouble <$> [C.exp| double { cv::getTickFrequency() }|]

-- | Returns the number of CPU ticks.
--
-- The function returns the current number of CPU ticks on some architectures
-- (such as x86, x64, PowerPC). On other platforms the function is equivalent to
-- 'getTickCount'. It can also be used for very accurate time measurements, as
-- well as for RNG initialization. Note that in case of multi-CPU systems a
-- thread, from which 'getCPUTickCount' is called, can be suspended and resumed
-- at another CPU with its own counter. So, theoretically (and practically) the
-- subsequent calls to the function do not necessary return the monotonously
-- increasing values. Also, since a modern CPU varies the CPU frequency
-- depending on the load, the number of CPU clocks spent in some code cannot be
-- directly converted to time units. Therefore, 'getTickCount' is generally a
-- preferable solution for measuring execution time.
getCPUTickCount :: IO Int64
getCPUTickCount = [C.exp| int64_t { cv::getCPUTickCount() }|]

{- | OpenCV will try to set the number of threads for the next parallel region.

If threads == 0, OpenCV will disable threading optimizations and run all its
functions sequentially. Passing threads < 0 will reset threads number to system
default. This function must be called outside of parallel region.

OpenCV will try to run its functions with specified threads number, but some
behaviour differs from framework:

  * __TBB__ – User-defined parallel constructions will run with the same threads
    number, if another is not specified. If late on user creates his own
    scheduler, OpenCV will be use it.

  * __OpenMP__ – No special defined behaviour.

  * __Concurrency__ – If threads == 1, OpenCV will disable threading
    optimizations and run its functions sequentially.

  * __GCD__ – Supports only values <= 0.

  * __C=__ – No special defined behaviour.
-}
setNumThreads :: Int32 -> IO ()
setNumThreads numThreads =
    [C.exp| void { cv::setNumThreads($(int32_t numThreads)) }|]

-- | Enables or disables the optimized code.
--
-- The function can be used to dynamically turn on and off optimized code (code
-- that uses 'SSE2, 'AVX', and other instructions on the platforms that support
-- it). It sets a global flag that is further checked by OpenCV functions. Since
-- the flag is not checked in the inner OpenCV loops, it is only safe to call
-- the function on the very top level in your application where you can be sure
-- that no other OpenCV function is currently executed.
--
-- By default, the optimized code is enabled unless you disable it in CMake. The
-- current status can be retrieved using 'useOptimized'.
setUseOptimized :: Bool -> IO ()
setUseOptimized on_off =
    [C.exp| void { cv::setUseOptimized($(int32_t c'on_off)) }|]
  where
    c'on_off :: Int32
    c'on_off = fromBool on_off

-- | Returns the status of optimized code usage.
--
-- The function returns @True@ if the optimized code is enabled. Otherwise, it
-- returns @False@.
useOptimized :: IO Bool
useOptimized = toBool <$> [C.exp| bool { cv::useOptimized() }|]
