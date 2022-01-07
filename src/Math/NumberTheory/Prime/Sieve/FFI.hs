{-# LANGUAGE CApiFFI #-}

-- |
-- Module      : Math.NumberTheory.Prime.Sieve.FFI
-- Copyright   : 2021 Preetham Gujjula
-- License     : BSD3
-- Maintainer  : primesieve-haskell@mail.preetham.io
-- Stability   : experimental
--
-- This module provides direct access to the C API of the primesieve library.
-- It's recommended that you use the higher-level interface in
-- "Math.NumberTheory.PrimeSieve#".
--
-- Documentation adapted from
-- [@primesieve.h@](https://primesieve.org/api/primesieve_8h.html).
-- and [@primesieve/iterator.h@](https://primesieve.org/api/iterator_8h.html)
module Math.NumberTheory.Prime.Sieve.FFI
  ( -- * From @primesieve.h@
    PrimesieveReturnCode (..),
    primesieve_generate_primes,
    primesieve_generate_n_primes,
    primesieve_nth_prime,
    primesieve_count_primes,
    primesieve_count_twins,
    primesieve_count_triplets,
    primesieve_count_quadruplets,
    primesieve_count_quintuplets,
    primesieve_count_sextuplets,
    primesieve_print_primes,
    primesieve_print_twins,
    primesieve_print_triplets,
    primesieve_print_quadruplets,
    primesieve_print_quintuplets,
    primesieve_print_sextuplets,
    primesieve_get_max_stop,
    primesieve_get_sieve_size,
    primesieve_get_num_threads,
    primesieve_set_sieve_size,
    primesieve_set_num_threads,
    primesieve_free,
    primesieve_free_ptr,
    primesieve_version,

    -- * From @primesieve/iterator.h@
    PrimesieveIterator,
    primesieve_iterator_size,
    primesieve_init,
    primesieve_free_iterator,
    primesieve_free_iterator_ptr,
    primesieve_skipto,
    primesieve_next_prime,
    primesieve_prev_prime,
  )
where

import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize (..), CInt (..))
import Foreign.Ptr (Ptr, FunPtr)

data PrimesieveReturnCode
  = SHORT_PRIMES
  | USHORT_PRIMES
  | INT_PRIMES
  | UINT_PRIMES
  | LONG_PRIMES
  | ULONG_PRIMES
  | LONGLONG_PRIMES
  | ULONGLONG_PRIMES
  | INT16_PRIMES
  | UINT16_PRIMES
  | INT32_PRIMES
  | UINT32_PRIMES
  | INT64_PRIMES
  | UINT64_PRIMES
  deriving (Enum, Show, Eq, Ord)

foreign import ccall unsafe "primesieve_generate_primes"
  primesieve_generate_primes :: Word64 -> Word64 -> Ptr CSize -> CInt -> IO (Ptr ())

foreign import ccall unsafe "primesieve_generate_n_primes"
  primesieve_generate_n_primes :: Word64 -> Word64 -> CInt -> IO (Ptr ())

foreign import ccall unsafe "primesieve_nth_prime"
  primesieve_nth_prime :: Int64 -> Word64 -> Word64

foreign import ccall unsafe "primesieve_count_primes"
  primesieve_count_primes :: Word64 -> Word64 -> Word64

foreign import ccall unsafe "primesieve_count_twins"
  primesieve_count_twins :: Word64 -> Word64 -> Word64

foreign import ccall unsafe "primesieve_count_triplets"
  primesieve_count_triplets :: Word64 -> Word64 -> Word64

foreign import ccall unsafe "primesieve_count_quadruplets"
  primesieve_count_quadruplets :: Word64 -> Word64 -> Word64

foreign import ccall unsafe "primesieve_count_quintuplets"
  primesieve_count_quintuplets :: Word64 -> Word64 -> Word64

foreign import ccall unsafe "primesieve_count_sextuplets"
  primesieve_count_sextuplets :: Word64 -> Word64 -> Word64

foreign import ccall unsafe "primesieve_print_primes"
  primesieve_print_primes :: Word64 -> Word64 -> IO ()

foreign import ccall unsafe "primesieve_print_twins"
  primesieve_print_twins :: Word64 -> Word64 -> IO ()

foreign import ccall unsafe "primesieve_print_triplets"
  primesieve_print_triplets :: Word64 -> Word64 -> IO ()

foreign import ccall unsafe "primesieve_print_quadruplets"
  primesieve_print_quadruplets :: Word64 -> Word64 -> IO ()

foreign import ccall unsafe "primesieve_print_quintuplets"
  primesieve_print_quintuplets :: Word64 -> Word64 -> IO ()

foreign import ccall unsafe "primesieve_print_sextuplets"
  primesieve_print_sextuplets :: Word64 -> Word64 -> IO ()

foreign import ccall unsafe "primesieve_get_max_stop"
  primesieve_get_max_stop :: Word64

foreign import ccall unsafe "primesieve_get_sieve_size"
  primesieve_get_sieve_size :: IO CInt

foreign import ccall unsafe "primesieve_get_num_threads"
  primesieve_get_num_threads :: IO CInt

foreign import ccall unsafe "primesieve_set_sieve_size"
  primesieve_set_sieve_size :: CInt -> IO ()

foreign import ccall unsafe "primesieve_set_num_threads"
  primesieve_set_num_threads :: CInt -> IO ()

foreign import ccall unsafe "primesieve_free"
  primesieve_free :: Ptr () -> IO ()

foreign import ccall unsafe "&primesieve_free"
  primesieve_free_ptr :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "primesieve_version"
  primesieve_version :: CString

--
-- From primesieve/iterator.h
--

data PrimesieveIterator

foreign import ccall unsafe "primesieve_iterator_size"
  primesieve_iterator_size :: CSize

foreign import ccall unsafe "primesieve_init"
  primesieve_init :: Ptr PrimesieveIterator -> IO ()

foreign import ccall unsafe "primesieve_free_iterator"
  primesieve_free_iterator :: Ptr PrimesieveIterator -> IO ()

foreign import ccall unsafe "&primesieve_free_iterator"
  primesieve_free_iterator_ptr :: FunPtr (Ptr PrimesieveIterator -> IO ())

foreign import ccall unsafe "primesieve_skipto"
  primesieve_skipto :: Ptr PrimesieveIterator -> Word64 -> Word64 -> IO ()

foreign import capi "primesieve/iterator.h primesieve_next_prime"
  primesieve_next_prime :: Ptr PrimesieveIterator -> IO Word64

foreign import capi "primesieve/iterator.h primesieve_prev_prime"
  primesieve_prev_prime :: Ptr PrimesieveIterator -> IO Word64
