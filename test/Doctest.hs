module Main where

import Build_doctests (flags, module_sources, pkgs)
import Test.DocTest

main :: IO ()
main = doctest (flags <> pkgs <> module_sources)
