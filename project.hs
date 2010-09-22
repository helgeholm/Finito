data Profile = Profile {
     id :: String,
     variants :: [String]
}

data Project = Project {
     workdir :: String,
     profiles :: [Profile],
     componentdirs :: [String],
     includepaths :: [String],
     libpaths :: [String],
     cppoptions :: [String],
     cppdefines :: [String]
}
