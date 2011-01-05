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

-- Given a set of defined variants and a condition, is the condition valid?
valid :: [Name] -> Cond -> Bool
valid = undefined

----------------------------------
-- TEST DATA

posate = SimpleOrRestrictor . SimpleAndRestrictor 
       . PosateRestrictor . NameRestrictor . Name
nameCond = SimpleCond . NameAtom . Name
aName = NameAtom . Name

-- test data
sampleProfiles :: [Assign]
sampleProfiles = [
  (ProperAssign (nameCond "color") [
      nameCond "red",
      nameCond "white"]),
  (ProperAssign (nameCond "taste") [
    nameCond "apple",
    RestrCond (aName "strawberry") (posate "red")]),
  (ProperAssign (RestrCond (aName "bleached") (posate "white")) [
      nameCond "yes",
      nameCond "no"])
  ]
