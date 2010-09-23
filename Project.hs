import Utils (trim, splitOn)

data Profile = Profile {
     id :: String,
     variants :: [String]
} deriving (Show, Eq)

data Project = Project {
     workdir :: String,
     profiles :: [Profile],
     componentdirs :: [String],
     includepaths :: [String],
     libpaths :: [String],
     cppoptions :: [String],
     cppdefines :: [String]
} deriving (Show, Eq)

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
