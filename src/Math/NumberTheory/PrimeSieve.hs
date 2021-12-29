-- |
-- Module      : Math.NumberTheory.PrimeSieve
-- Copyright   : 2021 Preetham Gujjula
-- License     : BSD3
-- Maintainer  : primesieve-haskell@mail.preetham.io
-- Stability   : experimental
--
-- This module provides a high-level, polymorphic interface to the primesieve
-- library. For a lower-level interface, see "Math.NumberTheory.PrimeSieve.FFI#".
module Math.NumberTheory.PrimeSieve
  ( primes,
  )
where

import Data.Word (Word64)
import Foreign.Marshal.Alloc (mallocBytes)
import Math.NumberTheory.PrimeSieve.FFI
  ( primesieveIteratorSize,
    primesieve_init,
    primesieve_next_prime
  )
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

iterateIO :: IO a -> IO [a]
iterateIO mx = (:) <$> mx <*> unsafeInterleaveIO (iterateIO mx)

primes :: [Word64]
primes = unsafePerformIO $ do
  iter <- mallocBytes primesieveIteratorSize
  primesieve_init iter
  iterateIO (primesieve_next_prime iter)
{-# NOINLINE primes #-}
