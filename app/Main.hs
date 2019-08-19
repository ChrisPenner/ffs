module Main where

import qualified Ex

main :: IO ()
main = appendFile "/Users/chris/fuse.log" "\n\n======\n\n" >> Ex.main
