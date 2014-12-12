import ExtractTypeSignature
import Test.HUnit
import System.IO
import Control.Monad.IO.Class

stringMockCode :: String
stringMockCode = "  typesig str8int2str: [String, Numeral => String]"
realMockCode :: [String]
{- realMockCode = readFile "typesig.rb" -}
realMockCode = lines "class RubypeTest\ndef str8int2Str(a,b)\n'#{a} b'\nend\ntypesig str8int2str: [String, Numeral => String]\ndef marry(people)\n # Your Ruby code as usual\nend\ntypesig marry: [People -> Any]\nend"

tests = [
        "typeSignatureMatch" ~: typeSignatureMatch typeSignaturePattern1 stringMockCode ~=? Just "str8int2str: [String, Numeral => String]",
        "extractTypeSignature" ~: extractTypeSignature typeSignaturePattern1 stringMockCode ~=? Just "str8int2str :: String -> Numeral -> String",
        "extractTypeSignatures" ~: extractTypeSignatures typeSignaturePattern1 realMockCode ~=? ["str8int2str :: String -> Numeral -> String", "marry :: People -> Any"]
        ]

main = do
    runTestTT $ TestList tests
