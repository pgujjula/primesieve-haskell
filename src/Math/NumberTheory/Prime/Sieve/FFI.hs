{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : Math.NumberTheory.Prime.Sieve.FFI
-- Copyright   : 2021 Preetham Gujjula
-- License     : BSD3
-- Maintainer  : libraries@mail.preetham.io
-- Stability   : experimental
--
-- This module provides direct access to the C API of the primesieve library.
-- It's recommended that you use the higher-level interface in
-- "Math.NumberTheory.PrimeSieve#".
--
-- Documentation adapted from
-- [@primesieve.h@](https://primesieve.org/api/primesieve_8h.html).
-- and [@primesieve/iterator.h@](https://primesieve.org/api/iterator_8h.html),
-- see those documents for more information.
module Math.NumberTheory.Prime.Sieve.FFI
  ( -- * From @primesieve.h@
    PrimesieveGenerateFormat (..),
    formatToCInt,
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
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.ForeignPtr (FinalizerPtr, ForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)

-- | The output formats supported by 'primesieve_generate_primes' and
--   'primesieve_generate_n_primes'.
data PrimesieveGenerateFormat
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

-- | Convert a 'PrimesieveGenerateFormat' code to a 'CInt' to be passed into
--   the @type@ field in 'primesieve_generate_primes' or
--   'primesieve_generate_n_primes'.
formatToCInt :: PrimesieveGenerateFormat -> CInt
formatToCInt = fromIntegral . fromEnum

-- | Get an array with the primes inside the interval @[start, stop]@.
foreign import ccall unsafe "primesieve_generate_primes"
  primesieve_generate_primes :: Word64 -> Word64 -> Ptr CSize -> CInt -> IO (Ptr ())

-- | Get an array with the first @n@ primes ≥ @start@.
foreign import ccall unsafe "primesieve_generate_n_primes"
  primesieve_generate_n_primes :: Word64 -> Word64 -> CInt -> IO (Ptr ())

-- | Find the @n@th prime.
foreign import ccall unsafe "primesieve_nth_prime"
  primesieve_nth_prime :: Int64 -> Word64 -> Word64

-- | Count the primes within the interval @[start, stop]@.
foreign import ccall unsafe "primesieve_count_primes"
  primesieve_count_primes :: Word64 -> Word64 -> Word64

-- | Count the twin primes within the interval @[start, stop]@.
foreign import ccall unsafe "primesieve_count_twins"
  primesieve_count_twins :: Word64 -> Word64 -> Word64

-- | Count the prime triplets within the interval @[start, stop]@.
foreign import ccall unsafe "primesieve_count_triplets"
  primesieve_count_triplets :: Word64 -> Word64 -> Word64

-- | Count the prime quadruplets within the interval @[start, stop]@.
foreign import ccall unsafe "primesieve_count_quadruplets"
  primesieve_count_quadruplets :: Word64 -> Word64 -> Word64

-- | Count the prime quintuplets within the interval @[start, stop]@.
foreign import ccall unsafe "primesieve_count_quintuplets"
  primesieve_count_quintuplets :: Word64 -> Word64 -> Word64

-- | Count the prime sextuplets within the interval @[start, stop]@.
foreign import ccall unsafe "primesieve_count_sextuplets"
  primesieve_count_sextuplets :: Word64 -> Word64 -> Word64

-- | Print the primes within the interval @[start, stop]@ to the standard
--   output.
foreign import ccall unsafe "primesieve_print_primes"
  primesieve_print_primes :: Word64 -> Word64 -> IO ()

-- | Print the twin primes within the interval @[start, stop]@ to the standard
--   output.
foreign import ccall unsafe "primesieve_print_twins"
  primesieve_print_twins :: Word64 -> Word64 -> IO ()

-- | Print the prime triplets within the interval @[start, stop]@ to the
--   standard output.
foreign import ccall unsafe "primesieve_print_triplets"
  primesieve_print_triplets :: Word64 -> Word64 -> IO ()

-- | Print the prime quadruplets within the interval @[start, stop]@ to the
--   standard output.
foreign import ccall unsafe "primesieve_print_quadruplets"
  primesieve_print_quadruplets :: Word64 -> Word64 -> IO ()

-- | Print the prime quintuplets within the interval @[start, stop]@ to the
--   standard output.
foreign import ccall unsafe "primesieve_print_quintuplets"
  primesieve_print_quintuplets :: Word64 -> Word64 -> IO ()

-- | Print the prime sextuplets within the interval @[start, stop]@ to the
--   standard output.
foreign import ccall unsafe "primesieve_print_sextuplets"
  primesieve_print_sextuplets :: Word64 -> Word64 -> IO ()

-- | Returns the largest valid stop number for primesieve (currently
--   @'maxBound' :: 'Word64'@).
foreign import ccall unsafe "primesieve_get_max_stop"
  primesieve_get_max_stop :: Word64

-- | Get the current set sieve size in KiB.
foreign import ccall unsafe "primesieve_get_sieve_size"
  primesieve_get_sieve_size :: IO CInt

-- | Get the current set number of threads.
foreign import ccall unsafe "primesieve_get_num_threads"
  primesieve_get_num_threads :: IO CInt

-- | Set the sieve size in KiB (kibibyte).
foreign import ccall unsafe "primesieve_set_sieve_size"
  primesieve_set_sieve_size :: CInt -> IO ()

-- | Set the number of threads for use in primesieve_count_* and
--   'primesieve_nth_prime'
foreign import ccall unsafe "primesieve_set_num_threads"
  primesieve_set_num_threads :: CInt -> IO ()

-- | Deallocate a primes array created using the 'primesieve_generate_primes'
--   or 'primesieve_generate_n_primes' functions.
foreign import ccall unsafe "primesieve_free"
  primesieve_free :: Ptr () -> IO ()

-- | A 'FinalizerPtr' corresponding to 'primesieve_free', useful for creating a
--  'ForeignPtr' to an array created by 'primesieve_generate_primes' or
--  'primesieve_generate_n_primes'.
foreign import ccall unsafe "&primesieve_free"
  primesieve_free_ptr :: FinalizerPtr ()

-- | Get the primesieve version number, in the form @"i.j"@.
foreign import ccall unsafe "primesieve_version"
  primesieve_version :: CString

--
-- From primesieve/iterator.h
--

-- | A data structure that allows for easily iterating over primes both forwards
--   and backwards.
data PrimesieveIterator

-- | Get the size in bytes of a 'PrimesieveIterator'.
foreign import ccall unsafe "primesieve_iterator_size"
  primesieve_iterator_size :: CSize

-- | Initialize the 'PrimesieveIterator' before first using it.
foreign import ccall unsafe "primesieve_init"
  primesieve_init :: Ptr PrimesieveIterator -> IO ()

-- | Free all memory associated with the 'PrimesieveIterator'.
foreign import ccall unsafe "primesieve_free_iterator"
  primesieve_free_iterator :: Ptr PrimesieveIterator -> IO ()

-- | A 'FinalizerPtr' corresponding to 'primesieve_free_iterator', useful for
--   creating a 'ForeignPtr' to a 'PrimesieveIterator'.
foreign import ccall unsafe "&primesieve_free_iterator"
  primesieve_free_iterator_ptr :: FinalizerPtr PrimesieveIterator

-- | Reset the 'PrimesieveIterator' to @start@.
foreign import ccall unsafe "primesieve_skipto"
  primesieve_skipto :: Ptr PrimesieveIterator -> Word64 -> Word64 -> IO ()

-- | Get the next prime. Returns @'maxBound' :: 'Word64'@ if next prime
--   @> 2^64@.
foreign import capi "primesieve/iterator.h primesieve_next_prime"
  primesieve_next_prime :: Ptr PrimesieveIterator -> IO Word64

-- | Get the previous prime.
foreign import capi "primesieve/iterator.h primesieve_prev_prime"
  primesieve_prev_prime :: Ptr PrimesieveIterator -> IO Word64
