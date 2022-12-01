module Main (main) where

import Lib

main :: IO ()
main = do
    content <- getContents
    print $ day1 content
