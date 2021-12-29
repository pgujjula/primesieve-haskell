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
  ( 
  )
where
