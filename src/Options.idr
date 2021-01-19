module Options

import Data.List
import Data.List1
import Data.Nat
import Data.Strings
import System.Console.GetOpt

public export
record Config where
  constructor MkConfig
  printHelp  : Bool
  verbosity  : Nat
  checkOnly  : Bool
  directory  : String
  extensions : List String

export
Show Config where
  show (MkConfig printHelp verbosity checkOnly directory extensions) =
       "MkConfig {"
    ++ "printHelp = "   ++ show printHelp
    ++ ",verbosity = "  ++ show (natToInteger verbosity)
    ++ ",checkOnly = "  ++ show checkOnly
    ++ ",directory = "  ++ directory
    ++ ",extensions = " ++ show extensions
    ++ "}"

defaultConfig : Config
defaultConfig = MkConfig { printHelp  = False
                         , verbosity  = 2
                         , checkOnly  = True
                         , directory  = "."
                         , extensions = ["idr"]
                         }

help : Config -> Config
help = record {printHelp = True}

adjVerbosity : (Nat -> Nat) -> Config -> Config
adjVerbosity f = record {verbosity $= f}

doFix : Bool -> Config -> Config
doFix b = record {checkOnly = not b}

setDir : String -> Config -> Config
setDir d = record {directory = d}

setExts : String -> Config -> Config
setExts s = record {extensions = forget (split (',' ==) s)}

descs : List $ OptDescr (Config -> Config)
descs = [ MkOpt ['h'] ["help"]      (NoArg help)
           "prints this help text"
        , MkOpt ['v'] ["verbose"]   (NoArg $ adjVerbosity S)
            "increase verbosity (default verbosity is 2)"
        , MkOpt ['q'] ["quiet"]     (NoArg $ adjVerbosity pred)
            "decrease verbosity (default verbosity is 2)"
        , MkOpt ['c'] ["check"]     (NoArg $ doFix False)
            "check and list files with issues (default)"
        , MkOpt ['f'] ["fix"]       (NoArg $ doFix True)
            "check and fix files with issues"
        , MkOpt ['d'] ["directory"] (ReqArg setDir "<directory>")
            "set source directory to process\n(default is the current directory)"
        , MkOpt ['e'] ["ext"] (ReqArg setExts "<extensions>")
            "comma separated list of file extensions\n(default is \"idr\")"
        ]

public export
applyArgs : List String -> Either (List String) Config
applyArgs args =
  case getOpt RequireOrder descs args of
       MkResult opts [] [] [] => Right $ foldl (flip apply) defaultConfig opts
       MkResult _ n u e       => Left $ map unknown (n ++ u) ++ e

  where unknown : String -> String
        unknown = ("Unknown option: " ++)

public export
info : String -> String
info pn = usageInfo ("Usage: " ++ pn ++ " [options]\n\nOptions:\n") descs
