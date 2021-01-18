module Options

import Data.List
import Data.List1
import Data.Strings
import System.Console.GetOpt

public export
data Option = Help
            | Verbose
            | Check
            | Fix
            | Quiet
            | Dir String
            | Ext (List String)

descs : List $ OptDescr Option
descs = [ MkOpt ['h'] ["help"]      (NoArg Help)
            "prints this help text"
        , MkOpt ['v'] ["verbose"]   (NoArg Verbose)
            "increase verbosity (default verbosity is 2)"
        , MkOpt ['q'] ["quiet"]     (NoArg Quiet)
            "decrease verbosity (default verbosity is 2)"
        , MkOpt ['c'] ["check"]     (NoArg Check)
            "check and list files with issues (default)"
        , MkOpt ['f'] ["fix"]       (NoArg Fix)
            "check and fix files with issues"
        , MkOpt ['d'] ["directory"] (ReqArg Dir "<directory>")
            "set source directory to process\n(default is the current directory)"
        , MkOpt ['e'] ["ext"] (ReqArg mkExt "<extensions>")
            "comma separated list of file extensions\n(default is \"idr\")"
        ]
  where mkExt : String -> Option
        mkExt = Ext . forget . split (',' ==)

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

public export
adjConfig : Config -> Option -> Config
adjConfig c Help     = record { printHelp  = True } c
adjConfig c Verbose  = record { verbosity  $= S } c
adjConfig c Quiet    = record { verbosity  $= (`minus` 1) } c
adjConfig c Check    = record { checkOnly  = True } c
adjConfig c Fix      = record { checkOnly  = False } c
adjConfig c (Dir d)  = record { directory  = d } c
adjConfig c (Ext es) = record { extensions = es } c

public export
applyArgs : List String -> Either (List String) Config
applyArgs args =
  case getOpt RequireOrder descs args of
       MkResult opts [] [] [] => Right $ foldl adjConfig defaultConfig opts
       MkResult _ n u e       => Left $ map unknown (n ++ u) ++ e

  where unknown : String -> String
        unknown = ("Unknown option: " ++)

public export
info : String -> String
info pn = usageInfo ("Usage: " ++ pn ++ " [options]\n\nOptions:\n") descs
