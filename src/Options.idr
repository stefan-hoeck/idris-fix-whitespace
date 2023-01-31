module Options

import Data.FilePath
import Data.List
import Data.List1
import Data.Nat
import Data.String
import Derive.Prelude
import Derive.Pretty

import System.GetOpts

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          Program Config
--------------------------------------------------------------------------------

export %inline
Pretty Body where
  prettyPrec _ = line . interpolate

export %inline
Pretty FilePath where
  prettyPrec _ = line . interpolate

||| Program Config
public export
record Config where
  constructor MkConfig

  ||| If True, prints only the program synopsis.
  printHelp     : Bool

  ||| Verbosity level. At the moment, 0 means total silence (errors are
  ||| still printed to stderr) while 4 is most talkative.
  verbosity     : Nat

  ||| True, if files should only be checked for issues
  ||| without fixing their content.
  checkOnly     : Bool

  ||| True, if all non-hidden files should be checked when
  ||| traversing directories.
  includeAll    : Bool

  ||| True, if hidden files should be checked when
  ||| traversing directories.
  includeHidden : Bool

  ||| List of file extensions to be checked when traversing
  ||| directories. If `includeAll` is set to `True`, this
  ||| is ignored.
  extensions    : List Body

  ||| List of files and directories to be checked.
  files         : List FilePath

%runElab derive "Config" [Show,Eq,Pretty]

init : List String -> Config
init fs = MkConfig {
    printHelp     = False
  , verbosity     = 2
  , checkOnly     = True
  , includeAll    = False
  , includeHidden = False
  , files         = case fs of [] => ["."]; _ => map fromString fs
  , extensions    = ["idr"]
  }

||| Tests, whether a file or directory is hidden and should
||| still be included.
export
includeDir : FilePath -> Config -> Bool
includeDir p c = not (isHidden p) || c.includeHidden

||| Tests, whether a file should be included when traversing
||| a directory.
export
includeFile : FilePath -> Config -> Bool
includeFile p c =
  let ext := extension p
   in c.includeAll || any ((ext ==) . Just) c.extensions

--------------------------------------------------------------------------------
--          Applying Command Line Args
--------------------------------------------------------------------------------

help : Config -> Config
help = {printHelp := True}

adjVerbosity : (Nat -> Nat) -> Config -> Config
adjVerbosity f = {verbosity $= f}

doFix : Bool -> Config -> Config
doFix b = {checkOnly := not b}

setExts : String -> Config -> Config
setExts s = {extensions := mapMaybe parse $ forget (split (',' ==) s)}

setAll : Config -> Config
setAll = {includeAll := True}

setHidden : Config -> Config
setHidden = {includeHidden := True}

descs : List $ OptDescr (Config -> Config)
descs = [ MkOpt ['h'] ["help"]      (NoArg help)
           "prints this help text\n "
        , MkOpt ['v'] ["verbose"]   (NoArg $ adjVerbosity S)
            "increase verbosity (default verbosity is 2)\n "
        , MkOpt ['q'] ["quiet"]     (NoArg $ adjVerbosity pred)
            "decrease verbosity (default verbosity is 2)\n "
        , MkOpt ['c'] ["check"]     (NoArg $ doFix False)
            "check and list files with issues (default)\n "
        , MkOpt ['f'] ["fix"]       (NoArg $ doFix True)
            "check and fix files with issues\n "
        , MkOpt ['e'] ["ext"]       (ReqArg setExts "<exts>")
            $ unlines [ "comma separated list of extensions of files"
                      , "to be included when traversing directories"
                      , "(default is \"idr\")."
                      , "Note: Files whose name starts with a dot will be"
                      , "ignored unless '--includeHidden' is set."
                      , ""
                      ]
        , MkOpt ['a'] ["all"]       (NoArg setAll)
            "include all non-hidden files when traversing directories\n "
        , MkOpt []    ["includeHidden"] (NoArg setHidden)
            $ unlines [ "include hidden files (name starts with a dot)"
                      , "when traversing directories"
                      , ""
                      ]
        ]

export
applyArgs : List String -> Either (List String) Config
applyArgs args =
  case getOpt RequireOrder descs args of
       MkResult os n  [] [] => Right $ foldl (flip apply) (init n) os
       MkResult _ _ u e       => Left $ map unknown u ++ e

  where unknown : String -> String
        unknown = ("Unknown option: " ++)

--------------------------------------------------------------------------------
--          Usage Info
--------------------------------------------------------------------------------

version : String
version = "1.1.0"

progName : String
progName = "fix_whitespace"

usage : String
usage = "Usage: " ++ progName ++ " [options] [FILES]\n\nOptions:\n"

synopsis : String
synopsis = unlines [
    progName ++ " version " ++ version
  , ""
  , "  Removes trailing whitespace characters from the specified"
  , "  text files making sure every text file ends with exactly one"
  , "  newline character. Windows style line breaks are replaced"
  , "  by Unix newline characters."
  , ""
  , "  If the passed file list contains directories, " ++ progName
  , "  will recursively adjust all files with the given extensions"
  , "  (see option --ext) in those directories. If no files are"
  , "  specified, the current directory will be traversed instead."
  , ""
  , usage
  ]

export
info : String
info = usageInfo synopsis descs

export
shortInfo : String
shortInfo = usageInfo usage descs
