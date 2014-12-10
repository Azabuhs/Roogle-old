type Document [String]

exstractTypeSignature :: String -> String -> Maybe String
exstractTypeSignature str ptn =
    | take $ length ptn $ str == ptn = str

-- if possible: let val = document
document :: Document
document = -- global variable

buildDocument :: [String] -> String -> Maybe [String]
buildDocument doc str =

appendToDocument :: String -> Maybe [String]
appendToDocument str = buildDocument document str

data Types a =  Ret a | Args (Types a) (Types a)

search :: Types 
search (Ret t) = -- return value
search (Args t t') = -- still argument

main = do
    document = extractTypeSignature undefiend "typesig" >>= appendToDocument
    putStrln document
