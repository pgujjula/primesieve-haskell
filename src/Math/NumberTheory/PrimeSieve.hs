{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
  ( -- * Strict prime generation
    Sievable,
    generatePrimes,
    generateNPrimes,

    -- * Prime counting
    nthPrime,
    countPrimes,
    countTwins,
    countTriplets,
    countQuadruplets,
    countQuintuplets,
    countSextuplets,

    -- * Prime printing
    printPrimes,
    printTwins,
    printTriplets,
    printQuadruplets,
    printQuintuplets,
    printSextuplets,

    -- * Lazy prime generation
    primes,
    primesTo,
    primesFrom,
    primesFromTo,

    -- * Sieve configuration
    getMaxStop,
    getSieveSize,
    getNumThreads,
    setSieveSize,
    setNumThreads,
  )
where

import Control.Monad (void)
import Data.Int (Int16, Int32, Int64)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr0)
import Data.Word (Word16, Word32, Word64)
import Foreign.C.Types
  ( CInt,
    CLLong,
    CLong,
    CShort,
    CSize,
    CUInt,
    CULLong,
    CULong,
    CUShort,
  )
import Foreign.ForeignPtr (addForeignPtrFinalizer, mallocForeignPtrBytes, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castFunPtr, castPtr)
import Foreign.Storable (Storable, peek)
import Math.NumberTheory.PrimeSieve.FFI
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

-- TODO: Look at ways to replace the typeclass with something closed.
-- I remember seeing in another library, them using type families or
-- data families for the same purpose (I think).

class (Integral a, Storable a) => Sievable a where
  primesieveReturnCode :: PrimesieveReturnCode

instance Sievable CShort where
  primesieveReturnCode = SHORT_PRIMES

instance Sievable CUShort where
  primesieveReturnCode = USHORT_PRIMES

instance Sievable CInt where
  primesieveReturnCode = INT_PRIMES

instance Sievable CUInt where
  primesieveReturnCode = UINT_PRIMES

instance Sievable CLong where
  primesieveReturnCode = LONG_PRIMES

instance Sievable CULong where
  primesieveReturnCode = ULONG_PRIMES

instance Sievable CLLong where
  primesieveReturnCode = LONGLONG_PRIMES

instance Sievable CULLong where
  primesieveReturnCode = ULONGLONG_PRIMES

instance Sievable Int16 where
  primesieveReturnCode = INT16_PRIMES

instance Sievable Word16 where
  primesieveReturnCode = UINT16_PRIMES

instance Sievable Int32 where
  primesieveReturnCode = INT32_PRIMES

instance Sievable Word32 where
  primesieveReturnCode = UINT32_PRIMES

instance Sievable Int64 where
  primesieveReturnCode = INT64_PRIMES

instance Sievable Word64 where
  primesieveReturnCode = UINT64_PRIMES

generatePrimes :: Sievable a => Word64 -> Word64 -> Vector a
generatePrimes = generatePrimes'

generatePrimes' :: forall a. Sievable a => Word64 -> Word64 -> Vector a
generatePrimes' start stop = unsafePerformIO $
  alloca $ \(sizePtr :: Ptr CSize) -> do
    let type' = fromIntegral (fromEnum (primesieveReturnCode @a))
    arrayPtr <- castPtr <$> primesieve_generate_primes start stop sizePtr type'
    size <- fromIntegral <$> peek sizePtr
    foreignPtr <- newForeignPtr (castFunPtr primesieve_free_ptr) arrayPtr
    pure (unsafeFromForeignPtr0 foreignPtr size)

generateNPrimes :: Sievable a => Word64 -> Word64 -> Vector a
generateNPrimes = generateNPrimes'

generateNPrimes' :: forall a. Sievable a => Word64 -> Word64 -> Vector a
generateNPrimes' n start = unsafePerformIO $ do
  let type' = fromIntegral (fromEnum (primesieveReturnCode @a))
  arrayPtr <- castPtr <$> primesieve_generate_n_primes n start type'
  foreignPtr <- newForeignPtr (castFunPtr primesieve_free_ptr) arrayPtr
  -- TODO: Safely truncate n
  pure (unsafeFromForeignPtr0 foreignPtr (fromIntegral n))

nthPrime :: Int64 -> Word64 -> Word64
nthPrime = primesieve_nth_prime

countPrimes :: Word64 -> Word64 -> Word64
countPrimes = primesieve_count_primes

countTwins :: Word64 -> Word64 -> Word64
countTwins = primesieve_count_twins

countTriplets :: Word64 -> Word64 -> Word64
countTriplets = primesieve_count_triplets

countQuadruplets :: Word64 -> Word64 -> Word64
countQuadruplets = primesieve_count_quadruplets

countQuintuplets :: Word64 -> Word64 -> Word64
countQuintuplets = primesieve_count_quintuplets

countSextuplets :: Word64 -> Word64 -> Word64
countSextuplets = primesieve_count_sextuplets

printPrimes :: Word64 -> Word64 -> IO ()
printPrimes = primesieve_print_primes

printTwins :: Word64 -> Word64 -> IO ()
printTwins = primesieve_print_twins

printTriplets :: Word64 -> Word64 -> IO ()
printTriplets = primesieve_print_triplets

printQuadruplets :: Word64 -> Word64 -> IO ()
printQuadruplets = primesieve_print_quadruplets

printQuintuplets :: Word64 -> Word64 -> IO ()
printQuintuplets = primesieve_print_quintuplets

printSextuplets :: Word64 -> Word64 -> IO ()
printSextuplets = primesieve_print_sextuplets

getMaxStop :: Word64
getMaxStop = primesieve_get_max_stop

getSieveSize :: IO CInt
getSieveSize = primesieve_get_sieve_size

getNumThreads :: IO CInt
getNumThreads = primesieve_get_num_threads

setSieveSize :: CInt -> IO ()
setSieveSize = primesieve_set_sieve_size

setNumThreads :: CInt -> IO ()
setNumThreads = primesieve_set_num_threads

primes :: [Word64]
primes = primesFrom 0

primesTo :: Word64 -> [Word64]
primesTo = primesFromTo 0

primesFrom :: Word64 -> [Word64]
primesFrom start = primesFromTo start primesieve_get_max_stop

primesFromTo :: Word64 -> Word64 -> [Word64]
primesFromTo start stop = unsafePerformIO $ do
  iterForeignPtr <- mallocForeignPtrBytes (fromIntegral primesieve_iterator_size)
  addForeignPtrFinalizer primesieve_free_iterator_ptr iterForeignPtr
  withForeignPtr iterForeignPtr $ \iterPtr -> do
    void (primesieve_init iterPtr)
    primesieve_skipto iterPtr start stop
  iterateIO stop (withForeignPtr iterForeignPtr primesieve_next_prime)

{-# NOINLINE primesFrom #-}

iterateIO :: Word64 -> IO Word64 -> IO [Word64]
iterateIO stop mx = do
  x <- mx
  if x > stop
    then pure []
    else (x :) <$> unsafeInterleaveIO (iterateIO stop mx)
