-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Debug.Trace
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Defines trace functions for debugging output.
--
-- This module exports `Debug.Trace.trace` and adds a few convenience
-- functions for debug tracing.
-- To turn off the debug trace, `ArM.NoTrace` can be imported instead
-- of this one.
--
-----------------------------------------------------------------------------

module ArM.Debug.Trace where

import qualified Debug.Trace as T

trace :: String -> a -> a
trace = T.trace
ttrace :: Show a => a -> a
ttrace x = trace (show x) x
strace :: String -> String
strace x = trace  x x
