module UniversalTM where

import TM
import Util
import Data.Char

utape :: String
utape = ['0','1','#',',','.',' '] -- temporary add ' ' for input

class UEncode a where
  bitenc :: a -> String -- over utape

bitChar :: Bool -> Char
bitChar True = '1'
bitChar False = '0'

instance UEncode Int where
  bitenc x = if x <= 0 then []
             else
               (bitChar $ x `mod` 2 == 1) : bitenc (x `div` 2)

instance UEncode Char where
  bitenc = bitenc . ord


instance UEncode Direction where
  bitenc GoLeft = "1"
  bitenc GoRight = "0"

instance UEncode Integer where
  bitenc x = bitenc (fromIntegral x :: Int)

instance (UEncode state, UEncode tape) => UEncode (Trans state tape) where
  bitenc (Trans st g d st' g') =
    dot st $
    dot g $
    dot d $
    dot st' $
    dot g' $ ""

{- convenience function to encode a to a string of 1s and 0s,
   and then append another string.  The Char u separates the
   two strings.-}
bitencSep :: UEncode a => Char -> a -> String -> String
bitencSep u x rest = bitenc x ++ (u:rest)

pound :: UEncode a => a -> String -> String
pound = bitencSep '#'
comma :: UEncode a => a -> String -> String
comma = bitencSep ','
dot :: UEncode a => a -> String -> String
dot = bitencSep '.'

list :: UEncode a => [a] -> String -> String
list xs rest = foldrGlue comma xs ('#' : rest)

bitencSep2 :: UEncode a => Char -> a -> String -> String
bitencSep2 u x rest = bitenc u ++bitenc x ++ (',':rest)
-- bitencSep2 u x rest = u : bitenc x ++ rest

blanks :: UEncode a => a -> String -> String
blanks = bitencSep2 ' '

list2 :: UEncode a => [a] -> String -> String
list2 xs rest = foldrGlue blanks xs ('#' : rest)


encodeh :: (UEncode input, UEncode state, UEncode tape) =>
          TM input state tape ->
          String {- string to follow this one -} ->
          String -- over Utape
encodeh (TM states inputs tapesyms _ blank leftend trans start final) rest =
  pound leftend $
  pound blank $
  pound start $
  list final $
  list trans rest
 
encode :: (UEncode input, UEncode state, UEncode tape) =>
          TM input state tape ->
          String -- over Utape
encode tm = encodeh tm ""


--addblanks :: UEncode a => [a] -> [a]
--  addblanks (_:xs) = bitenc x ++ xs
--  addblanks [] = []

-- turn a TM and an input string into a single input for the universal TM
inputU :: (UEncode input, UEncode state, UEncode tape) =>
          TM input state tape -> [input] -> String -- over Utape
inputU tm xs = encodeh tm (list2 xs "") where


--accepts :: (Eq state, Eq tape) => TM input state tape -> [input] -> Bool
--accepts tm xs n = acceptsh tm [initialConfig tm xs]
