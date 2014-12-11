module ExtractTypeSignature where
import System.Environment
import Control.Monad.IO.Class

type Document = [String]

{- it needs to return String instead of Maybe String -}
extractTypeSignature :: String -> String -> Maybe String
extractTypeSignature ptn str =
    if take (length ptn) str == ptn 
    then Just str
    else Nothing

extractTypeSignature' :: Stirng -> String -> String
extractTypeSignature' ptn str =
    if take 

compactMaybe :: [Maybe String] -> [String]
compactMaybe (Just x:xs) = [x] ++ compactMaybe xs
compactMaybe (Nothing:xs) = compactMaybe xs

extractTypeSignatures :: String -> [String] -> [String]
extractTypeSignatures ptn = compactMaybe $ map $ extractTypeSignatureWithSpecifiedPattern ptn

extractTypeSignatureWithSpecifiedPattern :: String -> String -> Maybe String
extractTypeSignatureWithSpecifiedPattern = extractTypeSignature

typeSignaturePattern1 :: String
typeSignaturePattern1 = "typesig"
