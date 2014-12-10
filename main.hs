import System.Environment

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

main = do args <- getArgs
          codes <- readFile $ head args-- read file to get the code using typesig
          doc <- extractTypeSignatures typeSignaturePattern1 codes
          -- somehow show the doc accumulated
