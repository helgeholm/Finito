module Profile where

import Data.Map
import Data.Set
import List

type VariantId = String
type ProfileId = String
type FixedProfiles = [VariantId] -- One selected variant per profile

--instance Show (FixedProfiles -> Bool) where
--  show _ = "VariantValidityPredicate"

data Variant = Variant {
     vid :: VariantId,
     depends :: [VariantId]
} deriving (Show)

data Profile = Profile {
     key :: String,
     variants :: [Variant]
} deriving (Show)
instance Ord (Profile) where
  compare a b = compare (key a) (key b)
instance Eq (Profile) where
  a == b = (key a) == (key b)

-- internal crap

testProfile1 = Profile "platform" [Variant "win32" [], Variant "yourmom" []]
testProfile2 = Profile "syslink" [Variant "32bit" ["win32", "debug"]]
testProfile3 = Profile "buildtype" [Variant "release" [], Variant "debug" []]
testProfiles :: [Profile]
testProfiles = [testProfile1, testProfile2, testProfile3]

profileOf :: [Profile] -> VariantId -> ProfileId
profileOf [] _ = undefined
profileOf (p:ps) v = if v `elem` pvars then key p
                                       else profileOf ps v
  where pvars = List.map vid (variants p)

dependsOn :: [Profile] -> Profile -> [ProfileId]
dependsOn ps p = unique dupDeps
  where unique = (Data.Set.toList . Data.Set.fromList)
        dupDeps = concat (List.map varDependsOn (variants p))
        varDependsOn v = List.map (profileOf ps) (depends v)

type MPP = Map ProfileId [ProfileId]

mkDepGraph :: [Profile] -> MPP
mkDepGraph ps = mkDepGraphR ps Data.Map.empty ps

mkDepGraphR :: [Profile] -> MPP -> [Profile] -> MPP
mkDepGraphR _ m [] = m
mkDepGraphR allPs m (p:ps) = Data.Map.union this recursive
  where this      = Data.Map.insert (key p) (dependsOn allPs p) m
        recursive = mkDepGraphR allPs m ps
