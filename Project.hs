import Utils (trim, splitOn)

data Project = Project {
     workdir :: String,
     profiles :: [Profile],
     componentdirs :: [String],
     includepaths :: [String],
     libpaths :: [String],
     cppoptions :: [String],
     cppdefines :: [String]
} deriving (Show, Eq)
