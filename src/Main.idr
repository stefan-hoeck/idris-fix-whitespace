module Main

import System.File

readErr : String -> FileError -> IO ()

writeErr : String -> FileError -> IO ()

modFile : (String -> String) -> String -> IO ()
modFile f path = do Right contents <- readFile path
                                   | Left err => readErr path err
                    Right ()       <- writeFile path (f contents)
                                   | Left err => writeErr path err

main : IO ()
main = putStrLn "Hello whitespace"
