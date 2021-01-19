module Main

import Options
import Fix

import Data.Strings

import Control.Monad.Either
import Control.Monad.Reader

import System
import System.File
import System.Directory
import System.Path

||| Error handling and config passing in one transformer stack
Prog : Type -> Type
Prog t = ReaderT Config (EitherT (List String) IO) t

run : Config -> Prog () -> IO ()
run c (MkReaderT f) = do Right () <- runEitherT (f c)
                                  | Left es => traverse_ putStrLn es
                         pure ()

--------------------------------------------------------------------------------
--          Logging
--------------------------------------------------------------------------------

log : Nat -> Lazy String -> Prog ()
log n msg = ask >>= \c => when (c.verbosity >= n) (putStrLn msg)

trace : Lazy String -> Prog ()
trace = log 4

debug : Lazy String -> Prog ()
debug = log 3

info : Lazy String -> Prog ()
info = log 2

warn : Lazy String -> Prog ()
warn = log 1

--------------------------------------------------------------------------------
--          Error Handling
--------------------------------------------------------------------------------

throw : List String -> Prog a
throw es = MkReaderT \_ => left es

throwOne : String -> Prog a
throwOne e = throw [e]

--------------------------------------------------------------------------------
--          File Handling
--------------------------------------------------------------------------------

tryFile :  (mod : String)
        -> (String -> Prog (Either FileError a))
        -> (pth : String)
        -> Prog a
tryFile mod p pth = do trace $ mod ++ " file " ++ pth
                       Right a <- p pth
                               |  Left e => throwOne $ "Error when " ++ mod
                                                     ++ " file " ++ pth ++ ": "
                                                     ++ show e
                       pure a

read : String -> Prog String
read = tryFile "reading" readFile

write : (pth : String) -> (content : String) -> Prog ()
write p c = tryFile "writing" (`writeFile` c) p

procFile : String -> Prog ()
procFile pth = do debug $ "Checking file " ++ pth
                  fix      <- map (not . checkOnly) ask
                  contents <- read pth
                  let newConts = transform contents
                  if newConts /= contents
                     then do info  $ pth ++ ": Found some issues."
                             when fix $ write pth newConts

                     else debug $ pth ++ ": No issues found."

--------------------------------------------------------------------------------
--          Scanning Directories
--------------------------------------------------------------------------------

printInfo : HasIO io => io ()
printInfo = putStrLn info

entries : (inDir : Bool) -> String -> Prog $ List String
entries inDir pth =
  do c <- ask
     trace ("Checking whether file or dir is hidden: " ++ pth)
     if (not inDir || includeDir pth c)
        then do trace $ "Trying to open directory " ++ pth
                Right d <- openDir pth
                  | Left _ => do trace ("Not a directory: " ++ pth)
                                 trace ("Checking whether to process " ++ pth)
                                 if (not inDir || includeFile pth c)
                                     then trace ("Including " ++ pth) $> [pth]
                                     else trace ("Ignoring " ++ pth)  $> []
                debug $ "Scanning directory " ++ pth
                run d
        else pure []


  where run : Directory -> Prog $ List String
        run d = do trace $ "Getting next entry from " ++ pth
                   Right e <- dirEntry d
                           | Left _ => do trace "No entries left"
                                          closeDir d
                                          pure []
                   trace $ "Found new entry: " ++ e
                   es <- run d
                   if e == "." || e == ".."
                      then pure es
                      else map (++ es) $ entries True (pth </> e)

--------------------------------------------------------------------------------
--          Main Program
--------------------------------------------------------------------------------

prog : Prog ()
prog = do c <- ask
          debug $ "Configuration: " ++ show c
          if c.printHelp
             then putStrLn info
             else traverse (entries False) c.files  >>=
                  traverse_ procFile . join

main : IO ()
main = do (pn :: args) <- getArgs
                       |  Nil => putStrLn "Missing executable name. Aborting..."
          Right config <- pure $ applyArgs args
                       | Left es => traverse_ putStrLn es *> putStrLn shortInfo
          run config prog
