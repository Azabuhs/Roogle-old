module ExtractTypeSignature where
import System.Environment
import Control.Monad.IO.Class
import Data.Char
import Text.Regex

type Document = [String]

extractTypeSignature :: String -> String -> Maybe String
extractTypeSignature ptn str = typeSignatureMatch ptn str

typeSignatureMatch :: String -> String -> Maybe String
typeSignatureMatch ptn str = case matchRegexAll (mkRegex $ ".*" ++ ptn) str of
                       Just (_, _, v, _) -> Just v
                       Nothing -> Nothing

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
typeSignaturePattern1 = "typesig "
