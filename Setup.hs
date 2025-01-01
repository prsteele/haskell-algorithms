{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- Handle installation with and without cabal-doctest available.

#ifndef MIN_VERSION_cabal_doctest
#define MIN_VERSION_cabal_doctest(x,y,z) 0
#endif

#if MIN_VERSION_cabal_doctest(1,0,0)
-- We have cabal-doctest

import Distribution.Extra.Doctest

main :: IO ()
main = defaultMainWithDoctests "doctests"
#else
-- We don't have cabal-doctest

import Distribution.Simple

main :: IO ()
main = defaultMain

#endif
