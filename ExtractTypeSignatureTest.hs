import ExtractTypeSignature
import Test.HUnit
import System.IO
import Control.Monad.IO.Class

stringMockCode :: String
stringMockCode = "  typesig str8int2str: [String, Numeral => String]"

tests = [
        "typeSignatureMatch" ~: typeSignatureMatch typeSignaturePattern1 stringMockCode ~=? Just "str8int2str: [String, Numeral => String]",
        "extractTypeSignature" ~: extractTypeSignature typeSignaturePattern1 stringMockCode ~=? Just "str8int2str :: String -> Numeral -> String"
        ]

main = do
    runTestTT $ TestList tests
