import IO

import Data
import Parser

readRoot :: IO [Section]
readRoot = do
  dataz <- getContents
  case parseRoot "<stdin>" dataz of
    Left  err  -> fail (show err)
    Right root -> return root

main = do
  root <- readRoot
  print root
