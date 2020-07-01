-- Capitaliser
-- Reads in a textfile, capitalises its contents, and write it out.

module Main where

import Data.Char (toUpper)
import System.IO

main :: IO ()
main = do
    putStr "Enter the file to read from: "
    hFlush stdout
    in_filename <- getLine
    putStr "Enter the file to write to: "
    hFlush stdout
    out_filename <- getLine

    handle <- openFile in_filename ReadMode
    contents <- hGetContents handle
    writeFile out_filename (map toUpper contents)
    hClose handle