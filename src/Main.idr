module Main

import Options
import Fix

import Data.FilePath
import Data.String

import Control.Monad.Either
import Control.Monad.Reader

import System
import System.File
import System.Directory

import Text.PrettyPrint.Bernardy

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

throwOne : String -> Prog a
throwOne e = throwError [e]

--------------------------------------------------------------------------------
--          File Handling
--------------------------------------------------------------------------------

tryFile :  (mod : String)
        -> (String -> Prog (Either FileError a))
        -> (pth : FilePath)
        -> Prog a
tryFile mod p pth = do
  trace $ "\{mod} \{pth}"
  Right a <- p (interpolate pth)
    |  Left e => throwOne $ "Error when \{mod} \{pth}: \{show e}"
  pure a

read : FilePath -> Prog String
read = tryFile "reading" readFile

write : (pth : FilePath) -> (content : String) -> Prog ()
write p c = tryFile "writing" (`writeFile` c) p

procFile : FilePath -> Prog ()
procFile pth = do
  debug $ "Checking file \{pth}"
  fix      <- map (not . checkOnly) ask
  contents <- read pth
  let newConts = transform contents
  if newConts /= contents
     then do
       info  $ "\{pth}: Found some issues."
       when fix $ write pth newConts
     else debug $ "\{pth}: No issues found."

--------------------------------------------------------------------------------
--          Scanning Directories
--------------------------------------------------------------------------------

printInfo : HasIO io => io ()
printInfo = putStrLn info

body : String -> Maybe Body
body "."  = Nothing
body ".." = Nothing
body s    = parse s

entries : (inDir : Bool) -> FilePath -> Prog $ List FilePath
entries inDir pth = do
  c <- ask
  trace ("Checking whether file or dir is hidden: \{pth}")
  if (not inDir || includeDir pth c)
    then do
      trace "Trying to open directory \{pth}"
      Right d <- openDir "\{pth}"
        | Left _ => do
            trace "Not a directory: \{pth}"
            trace "Checking whether to process \{pth}"
            if (not inDir || includeFile pth c)
                then trace "Including \{pth}" $> [pth]
                else trace "Ignoring \{pth}" $> []
      debug $ "Scanning directory \{pth}"
      run d
    else pure []

  where
    run : Directory -> Prog $ List FilePath
    run d = do
      trace "Getting next entry from \{pth}"
      Right (Just e) <- nextDirEntry d
        | _ => trace "No entries left" >> closeDir d $> []
      es <- run d
      case body e of
        Just b  => (++ es) <$> entries True (pth /> b)
        Nothing => pure es

--------------------------------------------------------------------------------
--          Main Program
--------------------------------------------------------------------------------

LL80 : LayoutOpts
LL80 = Opts 80

prog : Prog ()
prog = do
  c <- ask
  debug "Configuration: \{render LL80 $ pretty c}"
  if c.printHelp
    then putStrLn info
    else traverse (entries False) c.files  >>=
         traverse_ procFile . join

main : IO ()
main = do
  (pn :: args) <- getArgs
    |  Nil => die "Missing executable name. Aborting..."
  Right config <- pure $ applyArgs args
    | Left es => traverse_ putStrLn es *> die shortInfo
  run config prog
