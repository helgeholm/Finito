import Utils (trim, splitOn)

type ProfileFix = [String]

data Variant = Variant {
     vkey :: String,
     values :: [String]
} deriving (Show, Eq)

data VariantNode = VariantNode {
     variant :: Variant,
     dependsOn :: [VariantNode]
} deriving (Show, Eq)

data Profile = Profile {
     key :: String,
     variants :: [String]
} deriving (Show, Eq)

profiles :: ProfileFix -> [Variant] -> [ProfileFix]
profiles fs [] = [reverse fs]
profiles fs (v:vs) = concat subProfiles
  where
    subProfiles :: [[ProfileFix]] = map (\x -> profiles (x:fs) vs) fixes
    fixes :: [String] = map (\x -> (vkey v) ++ "=" ++ x) (values v)

profiles_test = (expected == actual)
  where
    actual = profiles [] [Variant "A" ["1","2","3"], Variant "B" ["4","5"]]
    expected = [["A=1","B=4"],["A=1","B=5"],["A=2","B=4"],["A=2","B=5"],["A=3","B=4"],["A=3","B=5"]

parse_profile :: String -> Profile
parse_profile s = Profile pId pVariants
  where
    pId = trim rpId
    pVariants = map trim $ splitOn ',' rpVariants
    [rpId, rpVariants] = take 2 $ splitOn '=' s

test_parse_profile_simple :: Bool
test_parse_profile_simple = (actual == expected)
  where
    actual = parse_profile "bitness = 32bit, 64bit"
    expected = Profile "bitness" ["32bit", "64bit"]
