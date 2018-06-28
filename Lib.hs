{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Language.Haskell.TH

factorial :: Int -> Q (TExp (() -> Int))
factorial n =
  if n == 0
    then [|| \ () -> 1 ||]
    else [|| \ () -> n * $$(factorial (n - 1))() ||]
