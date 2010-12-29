module Permutations where

import Data

find :: (Monad a) => [Section] -> String -> a Section
find [] id =
  fail ("no section named " ++ id)
find (s@(Section (Name name) _):ss) id =
  if name == id then return s
                else find ss id

permuteTest :: (Monad a) => [Section] -> a [String]
permuteTest ss = do
  profiles <- find ss "profiles"
  let (Section (Name n) _) = profiles
  return [n]
