-- Replace any and all usages of these with standard library
-- if you find equivalent functions there.

module Utils (trim, splitOn) where

import Data.Char (isSpace)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

splitOn :: Char -> String -> [String]
splitOn sep s = all ++ [last]
  where
    (all, last) = foldl acc ([], "") s
    acc (all, last) c =
      if (c == sep) then (all ++ [last], "")
                    else (all, last ++ [c])
