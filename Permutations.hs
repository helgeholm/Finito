module Permutations where

import Data
import Control.Monad
import List (intersperse)

-- Given a full configuration, yeild all valid configuration permutations.
permute :: (Monad a) => Configuration -> a [Configuration]
permute c@(Configuration _ ss) = do
  (Section _ profiles) <- find ss "profiles"
  let perm = permute_profiles profiles
  return $ map (\p -> constrain_configuration p c) perm

-- Yield all valid profiles, assuming [Assign] is the profile axes.  Axises?  Axii?
permute_profiles :: [Assign] -> [[Name]]
permute_profiles as =
  filter (profilesFit as) selections
  where selections :: [[Name]] 
        selections = zeroOrOneFromEach [] groupedNames
        groupedNames :: [[Name]]
        groupedNames = map (concat . assNames) as
        assNames (NotReallyAssign _) = []
        assNames (ProperAssign _ ns) = map extractName ns
        extractName (SimpleCond (NameAtom n))  = [n]
        extractName (RestrCond (NameAtom n) _) = [n]
        extractName _                          = []

show_configurations :: [Configuration] -> String
show_configurations cs =
  concat $ intersperse "\n\n" (map show cs)

constrain_configuration :: [Name] -> Configuration -> Configuration
constrain_configuration defs (Configuration name ss) =
  let ssc = map (constrain_section defs) ss in
  Configuration (name ++ " " ++ (show defs)) ssc

-- Apply profile to all conditionals and return conditional-free section.
constrain_section :: [Name] -> Section -> Section
constrain_section defs (Section name assigns) =
  let ac = concat $ map (constrain_assign defs) assigns in
  Section name ac

constrain_cond :: (Monad m) => [Name] -> Cond -> m Cond
constrain_cond defs a@(SimpleCond _) = return a
constrain_cond defs (RestrCond a o)  =
               if (evalOr defs o) then return (SimpleCond a)
                                  else fail "condition invalid by defs"

constrain_assign :: (Monad m) => [Name] -> Assign -> m Assign
constrain_assign defs (NotReallyAssign c) = do
  cc <- constrain_cond defs c
  return $ NotReallyAssign cc
constrain_assign defs (ProperAssign c vs) = do
  cc <- constrain_cond defs c
  let vcs = concat $ map (constrain_cond defs) vs
  return $ ProperAssign cc vcs

-- Returns all possible combinations.
zeroOrOneFromEach :: [a] -> [[a]] -> [[a]]
zeroOrOneFromEach acc [] = [acc]
zeroOrOneFromEach acc (g:gs) = x ++ xs
  where x = zeroOrOneFromEach acc gs
        xs = concat $ map (\x -> zeroOrOneFromEach (x:acc) gs) g

find :: (Monad a) => [Section] -> String -> a Section
find [] id = 
  fail ("no section named " ++ id)
find (s@(Section (Name name) _):ss) id =
  if name == id then return s
                else find ss id

-- Given a set of defined variants and a condition, is the condition valid?
valid :: [Name] -> Cond -> Bool
valid _ (SimpleCond _) = True
valid defs (RestrCond _ o) = evalOr defs o

evalOr :: [Name] -> OrRestrictor -> Bool
evalOr defs = evalOr_
  where evalOr_ (SimpleOrRestrictor a) = evalAnd a
        evalOr_ (ProperOrRestrictor a o) = (evalAnd a) || (evalOr_ o)
        evalAnd (SimpleAndRestrictor n) = evalNot n
        evalAnd (ProperAndRestrictor n a) = (evalNot n) && (evalAnd a)
        evalNot (NegateRestrictor inner) = (not . evalInner) inner
        evalNot (PosateRestrictor inner) = evalInner inner
        evalInner (NameRestrictor n) = n `elem` defs
        evalInner (ParenRestrictor o) = evalOr_ o

-- Do we have 1 and only 1 valid definition from this profile?
profileFit :: [Name] -> Assign -> Bool
profileFit _ (NotReallyAssign _) = False
profileFit defs (ProperAssign _ vars) =
  let validVarNames = (concat . map extractName) vars in
  length (filter (`elem` defs) validVarNames) == 1
  where extractName (SimpleCond (NameAtom n))    = [n]
        extractName (SimpleCond _)               = []
        extractName (RestrCond (StringAtom _) _) = []
        extractName r@(RestrCond (NameAtom n) _) =
          if valid defs r then [n]
                          else []

-- Is this list of defined variants a valid permutation of the profile values?
profilesFit :: [Assign] -> [Name] -> Bool
profilesFit as defs = (length defs == length validAssigns
                       && and (map (profileFit defs) validAssigns))
  where validAssigns = filter validAssP as
        validAssP (NotReallyAssign _) = False
        validAssP (ProperAssign c _)  = valid defs c

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
  not (valid [Name "derp", Name "burp"] derpOrHerpButNotBurp)
  ]
  where bleachedIfWhite = RestrCond (aName "bleached") (o_posate "white")
        derpOrHerpButNotBurp = RestrCond (aName "_") (
          SimpleOrRestrictor (ProperAndRestrictor derpOrHerp notBurp))
        notBurp = a_negate "burp"
        derpOrHerp = (PosateRestrictor . ParenRestrictor)
                     (ProperOrRestrictor (a_posate "herp") (o_posate "derp"))

-- for manual testing
sampleAssign :: Assign
sampleAssign = head $ tail sampleProfiles

sampleSection :: Section
sampleSection = Section (Name "SampleSection") sampleProfiles

sampleProfile :: [Name]
sampleProfile = [(Name "apple"), (Name "white")]

sampleProfiles :: [Assign]
sampleProfiles = [
  (ProperAssign (nameCond "color") [
      nameCond "red",
      nameCond "white"]),
  (ProperAssign (nameCond "taste") [
    nameCond "apple",
    RestrCond (aName "strawberry") (o_posate "red")]),
  (ProperAssign (RestrCond (aName "bleached") (o_posate "white")) [
      nameCond "yesbleached",
      nameCond "nobleached"])
  ]
