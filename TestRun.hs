import IO

import Data
import Parser
import Permutations

readRoot :: IO [Section]
readRoot = do
  dataz <- getContents
  case parseRoot "<stdin>" dataz of
    Left  err  -> fail (show err)
    Right root -> return root

main = do
  root <- readRoot
  derp <- permuteTest root
  print derp
