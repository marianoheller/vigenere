module Lib where

import Data.Char
import Data.List

applyOffset :: Int -> Int -> Int
applyOffset shift x =
  mod (x + shift) 26

shift :: Int -> Char -> Char
shift n =
  chr
    . (+) lowerBase
    . applyOffset n
    . toCharBase
    . ord

lowerBase :: Int
lowerBase = 97

upperBase :: Int
upperBase = 65

toCharBase :: Int -> Int
toCharBase x =
  if x > 90
    then x - lowerBase
    else x - upperBase

data Direction
  = Forwards
  | Backwards
  deriving (Eq)

unvigenere :: String -> String -> String
unvigenere = vig Backwards

-- (vigenere message key)
vigenere :: String -> String -> String
vigenere = vig Forwards

vig :: Direction -> String -> String -> String
vig _ "" _ = ""
vig _ x "" = x
vig direction body keyword = encoded
  where
    key = makeKey body keyword
    encoded = zipWith (shiftChar direction) body key

makeKey :: String -> String -> String
makeKey message keyword = go message keyCycle
  where
    keyCycle = cycle keyword
    go [] _ = []
    go (x : xs) (y : ys)
      | x == ' ' = ' ' : go xs (y : ys)
      | otherwise = y : go xs ys

shiftChar :: Direction -> Char -> Char -> Char
shiftChar _ ' ' _ = ' '
shiftChar direction char key = shift convert char
  where
    f = toCharBase . ord $ key
    convert = case direction of
      Forwards -> f
      Backwards -> negate f