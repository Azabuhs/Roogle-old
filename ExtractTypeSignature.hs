module ExtractTypeSignature where
import System.Environment
import Control.Monad.IO.Class
import Data.Char
import Text.Regex
import Data.Map (Map)

type Document = [String]
type TypeSignature = String
data Scope = Class String [Scope]
            | Module String [Scope]
            | Method String TypeSignature deriving (Show)
type Wait = String
-- head contexts is the current Context
-- as encounter Wait, tail contexts
-- Wait is "end" in most cases
type Context = Map Scope Wait
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
toDoc' (Class klass scopes) = unlines $ map (\x -> (klass ++ toDoc' x)) scopes
toDoc' (Module mdl scopes) = unlines $ map (\x -> (mdl ++ toDoc' x)) scopes
toDoc' (Method meth typesig) = "#" ++ meth ++ " :: " ++ typesig ++ "\n"

{- buildDoc :: String -> Document -}
{- buildDoc = foldl appendContext [] $ lines -}

{- appendContext :: Context -> String -> Context -}
{- appendContext = getContext -}

{- getContext :: String -> Context -}
{- getContext str -}
  {- | isJust $ matchRegexAll (mkRegex $ "class") str = case matchRegexAll (mkRegex $ "class") str of -}
    {- Just (_, _, v, _) -> Class v -}

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
