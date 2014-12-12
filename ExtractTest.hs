import ExtractTypeSignature
import Test.HUnit
import System.IO
import Control.Monad.IO.Class

tests = [
        "typeSignatureMatch" ~: typeSignatureMatch typeSignaturePattern1 "  typesig str8int2str: [String, Numeral => String]" ~=? Just "str8int2str: [String, Numeral => String]"
        ]

main = do
    runTestTT $ TestList tests
