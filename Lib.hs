{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Language.Haskell.TH

fibonacci :: Int -> Q (TExp (() -> Int))
fibonacci n =
  case n of
    0 -> [|| \ () -> 0 ||]
    1 -> [|| \ () -> 1 ||]
    n -> [|| \ () -> $$(fibonacci (n - 1)) () + $$(fibonacci (n - 2)) () ||]
