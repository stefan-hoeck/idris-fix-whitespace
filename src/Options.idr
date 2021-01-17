module Options

import Data.List

public export
data Option = Help | Verbose | Check | Fix

public export
options : List Option
options = [Help,Verbose,Check,Fix]

public export
record OptionDesc where
  constructor MkDesc
  short : String
  long  : String
  desc  : String

public export
desc : Option -> OptionDesc
desc Help    = MkDesc "-h" "--help"    "print this help text"
desc Verbose = MkDesc "-v" "--verbose" "increase verbosity"
desc Check   = MkDesc "-c" "--check"   "check and list files with issues"
desc Fix     = MkDesc "-f" "--fix"     "check and fix files with issues"

public export
readOption : String -> Either String Option
readOption s = maybe (Left s) Right $ find matches options
  where matches : Option -> Bool
        matches x = let MkDesc sh lo _ = desc x
                     in sh == s || lo == s

public export
readOptions : List String -> Either String (List Option)
readOptions = traverse readOption
