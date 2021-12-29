{-# LANGUAGE CApiFFI #-}

-- |
-- Module      : Math.NumberTheory.PrimeSieve
-- Copyright   : 2021 Preetham Gujjula
-- License     : BSD3
-- Maintainer  : primesieve-haskell@mail.preetham.io
-- Stability   : experimental
--
-- This module provides direct access to the C API of the primesieve library.
-- It's recommended that you use the higher-level interface in
-- "Math.NumberTheory.PrimeSieve#".
--
-- Documentation adapted from the [C API reference]
-- (https://github.com/kimwalisch/primesieve/blob/master/doc/libprimesieve.md#c-api-reference)
-- and [@primesieve.h@](https://github.com/kimwalisch/primesieve/blob/master/include/primesieve.h).
module Math.NumberTheory.PrimeSieve.FFI
  ( primesieve_count_primes,
    primesieve_count_twins,
    primesieve_count_triplets,
    primesieve_count_quadruplets,
    primesieve_count_quintuplets,
    primesieve_count_sextuplets,
    primesieve_print_primes,
    -- iterator
    PrimesieveIterator,
    primesieveIteratorSize,
    primesieve_init,
    primesieve_free_iterator,
    primesieve_skipto,
    primesieve_next_prime,
    primesieve_prev_prime,
  )
where

import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.Ptr (Ptr)

--
-- From primesieve.h
--

foreign import ccall unsafe "primesieve_count_primes"
  primesieve_count_primes :: Int64 -> Int64 -> Int64

foreign import ccall unsafe "primesieve_count_twins"
  primesieve_count_twins :: Int64 -> Int64 -> Int64

foreign import ccall unsafe "primesieve_count_triplets"
  primesieve_count_triplets :: Int64 -> Int64 -> Int64

foreign import ccall unsafe "primesieve_count_quadruplets"
  primesieve_count_quadruplets :: Int64 -> Int64 -> Int64

foreign import ccall unsafe "primesieve_count_quintuplets"
  primesieve_count_quintuplets :: Int64 -> Int64 -> Int64

foreign import ccall unsafe "primesieve_count_sextuplets"
  primesieve_count_sextuplets :: Int64 -> Int64 -> Int64

foreign import ccall unsafe "primesieve_print_primes"
  primesieve_print_primes :: Int64 -> Int64 -> IO ()

--
-- From primesieve/iterator.h
--

data PrimesieveIterator

primesieveIteratorSize :: Int
primesieveIteratorSize = 80

foreign import ccall unsafe "primesieve_init"
  primesieve_init :: Ptr PrimesieveIterator -> IO ()

foreign import ccall unsafe "primesieve_free_iterator"
  primesieve_free_iterator :: Ptr PrimesieveIterator -> IO ()

foreign import ccall unsafe "primesieve_skipto"
  primesieve_skipto :: Ptr PrimesieveIterator -> Word64 -> Word64 -> IO ()

foreign import capi "primesieve/iterator.h primesieve_next_prime"
  primesieve_next_prime :: Ptr PrimesieveIterator -> IO Word64

foreign import capi "primesieve/iterator.h primesieve_prev_prime"
  primesieve_prev_prime :: Ptr PrimesieveIterator -> IO Word64
