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
log n msg = do MkConfig _ verbosity _ _ _ <- ask
               if verbosity >= n then putStrLn msg else pure ()

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
--          Flire Handling
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

doProc : String -> Prog ()
doProc pth = do debug $ "Checking file " ++ pth
                fix      <- map (not . checkOnly) ask
                contents <- read pth
                let newConts = transform contents
                if newConts /= contents
                   then do info  $ pth ++ ": Found some issues."
                           when fix $ write pth newConts

                   else debug $ pth ++ ": No issues found."

procFile : String -> Prog ()
procFile pth = do trace $ "Checking whether to process file " ++ pth
                  exts  <- map extensions ask
                  if any (\e => ("." ++ e) `isSuffixOf` pth) exts
                     then doProc pth
                     else trace $ "Ignoring file " ++ pth

--------------------------------------------------------------------------------
--          Scanning Directories
--------------------------------------------------------------------------------

printInfo : HasIO io => io ()
printInfo = putStrLn $ info "fix_whitespace"

entries : String -> Prog $ List String
entries pth = do trace $ "Trying to open directory " ++ pth
                 Right d <- openDir pth
                         | Left _ => trace ("Not a directory: " ++ pth)
                                  *> trace ("Returning " ++ pth ++ " as file.")
                                  $> [pth]
                 debug $ "Scanning directory " ++ pth
                 run d

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
                      else map (++ es) $ entries (pth </> e)

prog : Prog ()
prog = do c@(MkConfig ph _ _ dir _) <- ask
          debug $ "Configuration: " ++ show c
          if ph then printInfo
                else entries dir >>= traverse_ procFile

main : IO ()
main = do (pn :: args) <- getArgs
                       |  Nil => putStrLn "Missing executable name. Aborting..."
          Right config <- pure $ applyArgs args
                       | Left es => traverse_ putStrLn es *> printInfo
          run config prog
