module Main

import System.File
import System.Directory
import System.Path

readErr : String -> FileError -> IO ()
readErr p e = putStrLn $ "Error when reading file " ++ p ++ ": " ++ show  e

writeErr : String -> FileError -> IO ()
writeErr p e = putStrLn $ "Error when writing file " ++ p ++ ": " ++ show  e

modFile : (String -> String) -> String -> IO ()
modFile f path = do Right contents <- readFile path
                                   | Left err => readErr path err
                    Right ()       <- writeFile path (f contents)
                                   | Left err => writeErr path err
                    pure ()

export
entries : String -> IO $ List String
entries pth = do Right d <- openDir pth | Left _ => pure [pth] 
                 run d
  where run : Directory -> IO $ List String
        run d = do Right e <- dirEntry d | Left _ => closeDir d *> pure []
                   es <- run d
                   if e == "." || e == ".."
                      then pure es
                      else map (++ es) $ entries (pth </> e)

export
listEntries : String -> IO ()
listEntries s = entries s >>= traverse_ printLn

main : IO ()
main = listEntries "/home/gundi/idris/Idris2"
