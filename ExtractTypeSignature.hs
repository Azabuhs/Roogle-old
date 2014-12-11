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

compactMaybe :: [Maybe a] -> [a]
compactMaybe [] = []
compactMaybe (x:xs) = case x of
  Just v -> [v] ++ compactMaybe xs
  Nothing -> compactMaybe xs

extractTypeSignatures :: String -> [String] -> [String]
extractTypeSignatures ptn strs = compactMaybe (map (\x -> (extractTypeSignature ptn x)) strs)

extractTypeSignatureWithSpecifiedPattern :: String -> (String -> Maybe String)
extractTypeSignatureWithSpecifiedPattern args = extractTypeSignature args

typeSignaturePattern1 :: String
typeSignaturePattern1 = "typesig"
