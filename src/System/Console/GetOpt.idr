||| Idris port of the corresponding Haskell module
|||
||| This should probably go somewhere else eventually.
module System.Console.GetOpt

import Data.List
import Data.Strings

||| What to do with options following non-options
public export
data ArgOrder a =
  ||| no option processing after first non-option
  RequireOrder                |
  ||| freely intersperse options and non-options
  Permute                     |
  ||| wrap non-options into options
  ReturnInOrder (String -> a)

export
Functor ArgOrder where
  map _ RequireOrder      = RequireOrder
  map _ Permute           = Permute
  map f (ReturnInOrder g) = ReturnInOrder (f . g)

||| Describes whether an option takes an argument or not, and if so
||| how the argument is injected into a value of type `a`.
public export
data ArgDescr a =
   ||| no argument expected
   NoArg                   a         |
   ||| option requires argument
   ReqArg (String       -> a) String |
   ||| optional argument
   OptArg (Maybe String -> a) String

export
Functor ArgDescr where
  map f (NoArg x)    = NoArg (f x)
  map f (ReqArg g x) = ReqArg (f . g) x
  map f (OptArg g x) = OptArg (f . g) x

||| Each `OptDescr` describes a single option.
||| 
||| The arguments to 'Option' are:
||| 
||| * list of short option characters
||| * list of long option strings (without \"--\")
||| * argument descriptor
||| * explanation of option for user
public export
record OptDescr a where
  constructor MkOpt
  ||| list of short option characters
  shortNames  : List Char
  ||| list of long option strings (without "--")
  longNames   : List String
  ||| argument descriptor
  argDescr    : ArgDescr a
  ||| explanation of option for user
  description : String

export
Functor OptDescr where
  map f = record { argDescr $= map f }

-- kind of cmd line arg (internal use only):
data OptKind a
   = Opt       a                --    an option
   | UnreqOpt  String           --    an un-recognized option
   | NonOpt    String           --    a non-option
   | EndOfOpts                  --    end-of-options marker (i.e. "--")
   | OptErr    String           --    something went wrong...

-- @TODO: This should probably be in base.
unzip3 : List (a,b,c) -> (List a, List b, List c)
unzip3 []              = ([], ([], []))
unzip3 ((a,b,c) :: xs) = let (as,bs,cs) = unzip3 xs
                          in (a::as,b::bs,c::cs)

--------------------------------------------------------------------------------
--          Printing Usage Info
--------------------------------------------------------------------------------

fmtShort : ArgDescr a -> Char -> String
fmtShort (NoArg  _   ) so = "-" ++ singleton so
fmtShort (ReqArg _ ad) so = "-" ++ singleton so ++ " " ++ ad
fmtShort (OptArg _ ad) so = "-" ++ singleton so ++ "[" ++ ad ++ "]"

fmtLong : ArgDescr a -> String -> String
fmtLong (NoArg  _   ) lo = "--" ++ lo
fmtLong (ReqArg _ ad) lo = "--" ++ lo ++ "=" ++ ad
fmtLong (OptArg _ ad) lo = "--" ++ lo ++ "[=" ++ ad ++ "]"

fmtOpt : OptDescr a -> List (String,String,String)
fmtOpt (MkOpt sos los ad descr) =
  let sosFmt = concat $ intersperse ", " (map (fmtShort ad) sos) 
      losFmt = concat $ intersperse ", " (map (fmtLong ad) los) 
   in case lines descr of
           []       => [(sosFmt,losFmt,"")]
           (h :: t) => (sosFmt,losFmt,h) :: map (\s => ("","",s)) t

||| Return a string describing the usage of a command, derived from
||| the header (first argument) and the options described by the 
||| second argument.
usageInfo : (header : String) -> List $ OptDescr a -> String
usageInfo header optDescr =
  let (ss,ls,ds)   = (unzip3 . concatMap fmtOpt) optDescr
      paste        = \x,y,z => "  " ++ x ++ "  " ++ y ++ "  " ++ z 
      table        = zipWith3 paste (sameLen ss) (sameLen ls) ds
   in unlines $ header :: table

  where flushLeft : Nat -> String -> String
        flushLeft n s = s ++ pack (replicate (n `minus` length s) ' ')

        sameLen : List String -> List String
        sameLen ss = let len = foldl (\n => max n . length) 0 ss
                      in map (flushLeft len) ss
