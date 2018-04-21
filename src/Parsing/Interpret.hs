module Parsing.Interpret where

import System.Environment
import Types.Stately
import Types.Expressions
import Data.List

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

splitDefine :: String -> (String, String)
splitDefine s = (x, y) where (x:y:ys) = wordsWhen (=='-') s
