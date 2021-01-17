module Fix

import Data.List
import Data.List1
import Data.Strings

%default total

NL : Char
NL = '\n'

dropWhile1 : (a -> Bool) -> List1 a -> List1 a
dropWhile1 f (h ::: t) = h ::: dropWhile f t

noTrailingSpace : List Char -> List Char
noTrailingSpace = reverse . dropWhile isSpace . reverse

noTrailingNewlines : List1 $ List Char -> List1 $ List Char
noTrailingNewlines = reverse . dropWhile1 (force . null) . cons [] . reverse

-- reimplemented since original `unlines` to be testable.
unlinesImpl : List1 $ List Char -> List Char
unlinesImpl ([] ::: []) = [NL]
unlinesImpl (h ::: t)   = h ++ run t
  where run : List $ List Char -> List Char
        run []          = []
        run (cs :: css) = NL :: cs ++ run css


transformImpl : List Char -> List Char
transformImpl = unlinesImpl
              . noTrailingNewlines 
              . map noTrailingSpace 
              . split isNL

||| Transforms the given string in the following way:
|||  * on every line, trailing whitespace characters are removed
|||  * makes sure the string ends with exactly one newline character
|||
||| Note: So far, a newline character corresponds to `'\n'`.
export
transform : String -> String
transform = fastPack . transformImpl . fastUnpack

--------------------------------------------------------------------------------
--          Tests
--------------------------------------------------------------------------------

TestNoTrailingSpace : String -> String
TestNoTrailingSpace = pack . noTrailingSpace . unpack

noTrailingTest0 : TestNoTrailingSpace "" = ""
noTrailingTest0 = Refl

noTrailingTest1 : TestNoTrailingSpace "a test   " = "a test"
noTrailingTest1 = Refl

noTrailingTest2 : TestNoTrailingSpace "a test\t \t" = "a test"
noTrailingTest2 = Refl

noTrailingTest3 : TestNoTrailingSpace "a test" = "a test"
noTrailingTest3 = Refl

TestTransformImpl : String -> String
TestTransformImpl = pack . transformImpl . unpack

transTest0 : TestTransformImpl "" = "\n"
transTest0 = Refl

transTest1 : TestTransformImpl "test" = "test\n"
transTest1 = Refl

transTest2 : TestTransformImpl "test\n\n" = "test\n"
transTest2 = Refl

transTest3 : TestTransformImpl "test\ntrailing   \n\n" = "test\ntrailing\n"
transTest3 = Refl

transTest4 : TestTransformImpl "test\ntrailing   \t" = "test\ntrailing\n"
transTest4 = Refl

Empties : List String
Empties = [ ""
          , "\n"
          , "\n\n"
          , "\n\n\n"
          , " \n  \n\n"
          , " \n  \n\n\t"
          ]

testEmpties : map TestTransformImpl Empties = replicate (length Empties) "\n"
testEmpties = Refl
