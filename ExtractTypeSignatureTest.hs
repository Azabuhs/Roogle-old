import ExtractTypeSignature
import Test.HUnit
import System.IO
import Control.Monad.IO.Class

mockTypesignature = "  typesig str8int2str: [String, Numeral => String]" :: String
mockCode = lines "class RubypeTest\ndef str8int2str(a,b)\n'#{a} b'\nend\ntypesig str8int2str: [String, Numeral => String]\ndef marry(people)\n # Your Ruby code as usual\nend\ntypesig marry: [People -> Any]\nend" :: [String]
mockScope = Class "RubypeTest" [Method "str8int2str" "String -> Numeral -> String", Method "marry" "People -> Any"] :: Scope


tests = [
        "typeSignatureMatch" ~: typeSignatureMatch typeSignaturePattern1 mockTypesignature ~=? Just "str8int2str: [String, Numeral => String]",
        "extractTypeSignature" ~: extractTypeSignature typeSignaturePattern1 mockTypesignature ~=? Just "str8int2str :: String -> Numeral -> String",
        "extractTypeSignatures" ~: extractTypeSignatures typeSignaturePattern1 mockCode ~=? ["str8int2str :: String -> Numeral -> String", "marry :: People -> Any"],
        "ScopeToDoc" ~: lines (scopeToDoc mockScope) ~=? ["RubypeTest#str8int2str :: String -> Numeral -> String", "RubypeTest#marry :: People -> Any"]
        ]

main = do
    runTestTT $ TestList tests
