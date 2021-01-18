module Options

import Data.List

public export
data Option = Help | Verbose | Check | Fix | Quiet

public export
options : List Option
options = [Help,Verbose,Check,Fix]

public export
record OptionDesc where
  constructor MkDesc
  short : Char
  long  : String
  desc  : String

public export
desc : Option -> OptionDesc
desc Help    = MkDesc 'h' "--help"    "print this help text"
desc Verbose = MkDesc 'v' "--verbose" "increase verbosity"
desc Check   = MkDesc 'c' "--check"   "check and list files with issues"
desc Fix     = MkDesc 'f' "--fix"     "check and fix files with issues"
desc Quiet   = MkDesc 'q' "--quiet"   "decrease verbosity"

public export
readOptions : List String -> Either String (List Option)
readOptions = map concat . traverse read
  where read : String -> Either String (List Option)

public export
record Config where
  constructor MkConfig
  printHelp : Bool
  verbosity : Nat
  checkOnly : Bool

defaultConfig : Config
defaultConfig = MkConfig {printHelp = False, verbosity = 1, checkOnly = True}

decreaseNat : Nat -> Nat
decreaseNat 0     = 0
decreaseNat (S k) = k

public export
adjConfig : Option -> Config -> Config
adjConfig Help      = record { printHelp = True }
adjConfig Verbose   = record { verbosity $= S }
adjConfig Quiet     = record { verbosity $= decreaseNat }
adjConfig Check     = record { checkOnly = True }
adjConfig Fix       = record { checkOnly = False }

public export
applyArgs : List String -> Either String Config
