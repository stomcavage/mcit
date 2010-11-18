{-
       File: Text.hs
     Author: Steven Tomcavage (stomcava@seas.upenn.edu)
       Date: November, 2010
Description: Getting started with Haskell
-}

import Data.Char

-- Returns a list of the occurance of each alphabetic letter in the string.
countLetters str = map
        (\ch -> length [x | x <- str, elem x [ch, (toUpper ch)]])
        ['a'..'z']

-- Replaces a character using a list of tuples as a map
trChar lst ch
        | lst == []            = ch
        | fst (head lst) == ch = snd (head lst)
        | otherwise            = trChar (tail lst) ch

-- Replaces each occurance of all chars in the search string with the 
-- corresponding char in the replace string
tr search replace str = map (trChar (zip search replace)) str

-- Returns a lower-case alphabet left-shifted by pos
rotateLower pos = map
        (\n -> chr ((ord 'a') + (mod (n + pos) 26)))
        (take 26 [0..])

-- Encodes a string using Rot13 encoding 
rot13 str = tr 
        (concat [['a'..'z'], ['A'..'Z']])
        (concat [(rotateLower 13), (map toUpper (rotateLower 13))])
        str 

-- Basic tests for the functions in this file
test = countLetters "The quick brown fox jumps over the lazy dog." == 
        [1,1,1,1,3,1,1,2,1,1,1,1,1,1,4,1,1,2,1,2,2,1,1,1,1,1] &&
        trChar [('a','b'),('b','c'),('c','d')] 'b' == 'c' &&
        tr ['a','b','j','e','c','t'] ['h','o','r','r','o','r'] "abject" == 
        "horror" && 
        rotateLower 13 == "nopqrstuvwxyzabcdefghijklm" &&
        rot13 "nowhere!ABJURER" == "abjurer!NOWHERE"
        