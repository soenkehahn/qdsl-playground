{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Language.Haskell.TH

power :: Int -> Q (TExp (Float -> Float))
power n =
  if n < 0 then
    [|| \ x -> if x == 0
      then 0
      else 1 / ($$(power (-n)) x) ||]
  else if n == 0 then
    [|| \ x -> 1 ||]
  else if even n then
    [|| \ x ->
      let y = $$(power (n `div` 2)) x
      in y * y ||]
  else
    [|| \ x -> x * ($$(power (n - 1)) x) ||]
