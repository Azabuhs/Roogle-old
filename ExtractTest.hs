import ExtractTypeSignature
import Test.HUnit
import System.IO
import Control.Monad.IO.Class

test1 = assertEqual "typesignature is extracted" (extractTypeSignatures typeSignaturePattern1 $ lines $ liftIO $ readFile "./typesig.rb") "typesig str8int2: [String, Numeral => String]"

tests = TestList [TestLabel test1 test1]

main = do
    runTestText (putTextToHandle stderr False) tests
