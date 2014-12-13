module ExtractTypeSignature where
import System.Environment
import Control.Monad.IO.Class
import Data.Char
import Text.Regex
import Data.Map (Map)

type TypeSignature = String
data Scope = Class String Scope
            | Module String Scope
            | Method String TypeSignature deriving (Show)
type Wait = String
-- head contexts is the current Context
-- as encounter Wait, tail contexts
-- Wait is "end" in most cases
type Context = [Map Scope Wait]
type Contexts = [Context]

extractTypeSignature :: String -> String -> Maybe String
extractTypeSignature ptn str = case typeSignatureMatch ptn str of
    Just v -> Just $ toDoc v
    Nothing  -> Nothing

toDoc :: String -> String
toDoc str = subRegex (mkRegex "=>")
          (subRegex (mkRegex ",")
            (subRegex (mkRegex "\\[")
              (subRegex (mkRegex "\\]")
                (subRegex (mkRegex ":") str
                  " ::") "") "") " ->") "->"

toDoc' :: Scope -> String
toDoc' (Class klass scope) = klass ++ toDoc' scope
toDoc' (Module mdl scope) = mdl ++ toDoc' scope
toDoc' (Method meth typesig) = "#" ++ meth ++ " :: " ++ typesig

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
extractTypeSignatureWithSpecifiedPattern = extractTypeSignature

typeSignaturePattern1 :: String
typeSignaturePattern1 = "typesig "
