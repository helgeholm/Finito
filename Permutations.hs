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
valid _ (SimpleCond _) =
  True
valid defs (RestrCond _ or) = evalOr or
  where evalOr (SimpleOrRestrictor a) = evalAnd a
        evalOr (ProperOrRestrictor a o) = (evalAnd a) || (evalOr o)
        evalAnd (SimpleAndRestrictor n) = evalNot n
        evalAnd (ProperAndRestrictor n a) = (evalNot n) && (evalAnd a)
        evalNot (NegateRestrictor inner) = (not . evalInner) inner
        evalNot (PosateRestrictor inner) = evalInner inner
        evalInner (NameRestrictor n) = n `elem` defs
        evalInner (ParenRestrictor o) = evalOr o

----------------------------------
-- TEST DATA

a_posate = SimpleAndRestrictor . PosateRestrictor . NameRestrictor . Name
o_posate = SimpleOrRestrictor . a_posate
a_negate = SimpleAndRestrictor . NegateRestrictor . NameRestrictor . Name
o_negate = SimpleOrRestrictor . a_negate
nameCond = SimpleCond . NameAtom . Name
aName = NameAtom . Name

test_valid :: Bool
test_valid = and [
  valid [Name "derp", Name "white"] bleachedIfWhite,
  not (valid [Name "derp"] bleachedIfWhite),
  valid [Name "derp"] derpOrHerpButNotBurp,
  valid [Name "herp"] derpOrHerpButNotBurp,
  valid [Name "derp", Name "herp"] derpOrHerpButNotBurp,
  not (valid [Name "derp", Name "burp"] derpOrHerpButNotBurp)]
  where bleachedIfWhite = RestrCond (aName "bleached") (o_posate "white")
        derpOrHerpButNotBurp = RestrCond (aName "_") (
          SimpleOrRestrictor (ProperAndRestrictor derpOrHerp notBurp))
        notBurp = a_negate "burp"
        derpOrHerp = (PosateRestrictor . ParenRestrictor)
                     (ProperOrRestrictor (a_posate "herp") (o_posate "derp"))

-- for manual testing
sampleProfiles :: [Assign]
sampleProfiles = [
  (ProperAssign (nameCond "color") [
      nameCond "red",
      nameCond "white"]),
  (ProperAssign (nameCond "taste") [
    nameCond "apple",
    RestrCond (aName "strawberry") (o_posate "red")]),
  (ProperAssign (RestrCond (aName "bleached") (o_posate "white")) [
      nameCond "yes",
      nameCond "no"])
  ]
