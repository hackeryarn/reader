{-# LANGUAGE InstanceSigs #-}

module Lib where

import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

tupled :: String -> (String, String)
tupled = rev >>= (\a ->
          cap >>= (\b ->
            return (a, b)))

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) =
    Reader $ f . ra
