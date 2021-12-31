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
  ( Sievable (..),
    generatePrimes,
    generateNPrimes,
    nthPrime,
    countPrimes,
    countTwins,
    countTriplets,
    countQuadruplets,
    countQuintuplets,
    countSextuplets,
    primes,
    primesTo,
    primesFrom,
    primesFromTo,
  )
where

import Control.Monad (void)
import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy (Proxy))
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
  returnType :: Proxy a -> PrimesieveReturnType

getTypeCode :: Sievable a => Proxy a -> CInt
getTypeCode = fromIntegral . fromEnum . returnType

instance Sievable CShort where
  returnType _ = SHORT_PRIMES

instance Sievable CUShort where
  returnType _ = USHORT_PRIMES

instance Sievable CInt where
  returnType _ = INT_PRIMES

instance Sievable CUInt where
  returnType _ = UINT_PRIMES

instance Sievable CLong where
  returnType _ = LONG_PRIMES

instance Sievable CULong where
  returnType _ = ULONG_PRIMES

instance Sievable CLLong where
  returnType _ = LONGLONG_PRIMES

instance Sievable CULLong where
  returnType _ = ULONGLONG_PRIMES

instance Sievable Int16 where
  returnType _ = INT16_PRIMES

instance Sievable Word16 where
  returnType _ = UINT16_PRIMES

instance Sievable Int32 where
  returnType _ = INT32_PRIMES

instance Sievable Word32 where
  returnType _ = UINT32_PRIMES

instance Sievable Int64 where
  returnType _ = INT64_PRIMES

instance Sievable Word64 where
  returnType _ = UINT64_PRIMES

generatePrimes :: forall a. Sievable a => Word64 -> Word64 -> IO (Vector a)
generatePrimes start stop = alloca $ \(sizePtr :: Ptr CSize) -> do
  let typeCode = getTypeCode @a Proxy
  arrayPtr <- castPtr <$> primesieve_generate_primes start stop sizePtr typeCode
  size <- fromIntegral <$> peek sizePtr
  foreignPtr <- newForeignPtr (castFunPtr primesieve_free_ptr) arrayPtr
  pure (unsafeFromForeignPtr0 foreignPtr size)

generateNPrimes :: forall a. Sievable a => Word64 -> Word64 -> IO (Vector a)
generateNPrimes n start = do
  let typeCode :: CInt
      typeCode = getTypeCode @a Proxy
  arrayPtr <- castPtr <$> primesieve_generate_n_primes n start typeCode
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

primes :: [Word64]
primes = primesFrom 0

primesTo :: Word64 -> [Word64]
primesTo = primesFromTo 0

primesFrom :: Word64 -> [Word64]
primesFrom start = primesFromTo start primesieve_get_max_stop

primesFromTo :: Word64 -> Word64 -> [Word64]
primesFromTo start stop = unsafePerformIO $ do
  iterForeignPtr <- mallocForeignPtrBytes (fromIntegral primesieveIteratorSize)
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
