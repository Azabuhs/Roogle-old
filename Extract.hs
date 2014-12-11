import System.Environment
import Control.Monad.IO.Class

type Document = [String]

extractTypeSignature :: String -> String -> Maybe String
extractTypeSignature str ptn =
    if take (length ptn) str == ptn 
    then Just str
    else Nothing

extractTypeSignatures :: String -> [String] -> [Maybe String]
extractTypeSignatures ptn = map $ extractTypeSignatureWithSpecifiedPattern ptn

extractTypeSignatureWithSpecifiedPattern :: String -> String -> Maybe String
extractTypeSignatureWithSpecifiedPattern ptn str = extractTypeSignature str ptn

typeSignaturePattern1 :: String
typeSignaturePattern1 = "typesig"

main = do
    codes <- liftIO $ readFile "./typesig.rb"
    doc <- extractTypeSignatures typeSignaturePattern1 codes
    show doc
