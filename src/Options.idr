module Options

import Data.List
import System.Console.GetOpt

public export
data Option = Help | Verbose | Check | Fix | Quiet

descs : List $ OptDescr Option
descs = [ MkOpt ['h'] ["help"]    (NoArg Help)    "prints this help text"
        , MkOpt ['v'] ["verbose"] (NoArg Verbose) "increase verbosity"
        , MkOpt ['q'] ["quiet"]   (NoArg Quiet)   "decrease verbosity"
        , MkOpt ['c'] ["check"]   (NoArg Quiet)   "check and list files with issues (default)"
        , MkOpt ['f'] ["fix"]     (NoArg Quiet)   "check and fix files with issues"
        ]

public export
record Config where
  constructor MkConfig
  printHelp : Bool
  verbosity : Nat
  checkOnly : Bool

defaultConfig : Config
defaultConfig = MkConfig {printHelp = False, verbosity = 1, checkOnly = True}

public export
adjConfig : Config -> Option -> Config
adjConfig c Help      = record { printHelp = True } c
adjConfig c Verbose   = record { verbosity $= S } c
adjConfig c Quiet     = record { verbosity $= (`minus` 1) } c
adjConfig c Check     = record { checkOnly = True } c
adjConfig c Fix       = record { checkOnly = False } c

public export
applyArgs : List String -> Either (List String) Config
applyArgs args =
  case getOpt RequireOrder descs args of
       MkResult opts [] [] [] => Right $ foldl adjConfig defaultConfig opts
       MkResult _ n u e       => Left $ map unknown (n ++ u) ++ e

  where unknown : String -> String
        unknown = ("Unknown option: " ++)
