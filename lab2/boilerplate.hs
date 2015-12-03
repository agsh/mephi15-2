tokenize :: String -> [Token]
tokenize string = 
  let
    parseString acc ('\'':'"':xs) = parseString (acc ++ "\"") xs
    -- \n, \t, ...
    parseString acc ('"':xs) = (acc, xs)
    parseString acc (x:xs) = parseString (acc ++ [x]) xs
    parseString acc [] = error "!"--(acc, [])

    parseNumber acc (x:xs)
      | x `elem` ['}',']',','] = (acc, (x:xs))
      | otherwise = parseNumber (acc ++ [x]) xs
    parseNumber acc [] = error "!"--(acc, [])

    tokenize' [] tokens = tokens
    tokenize' (' ':xs) tokens = tokenize' xs tokens
    tokenize' ('n':'u':'l':'l':xs) tokens = tokenize' xs (TNull:tokens)
    tokenize' ('t':'r':'u':'e':xs) tokens = tokenize' xs (TBoolean True:tokens)
    tokenize' ('f':'a':'l':'s':'e':xs) tokens = tokenize' xs (TBoolean False:tokens)
    tokenize' ('{':xs) tokens = tokenize' xs (TOpenBrace:tokens) 
    tokenize' ('}':xs) tokens = tokenize' xs (TCloseBrace:tokens)
    tokenize' ('[':xs) tokens = tokenize' xs (TOpenBracket:tokens)
    tokenize' (']':xs) tokens = tokenize' xs (TCloseBracket:tokens)
    tokenize' (':':xs) tokens = tokenize' xs (TColon:tokens)
    tokenize' (',':xs) tokens = tokenize' xs (TComma:tokens)
    tokenize' ('"':xs) tokens = 
      let (s, xss) = parseString "" xs
      in tokenize' xss ((TString s):tokens)
    tokenize' xs tokens =
      let (s, xss) = parseNumber "" xs
      in tokenize' xss ((TNumber (read s :: Integer)):tokens)
  in reverse $ tokenize' string []
 
 -- работа с Data.Map: http://learnyouahaskell.com/modules#data-map
 
  
  data JSON = Null
            | Boolean Bool
            | String String
            | Number Integer
            | Array [JSON]
            | Object (M.Map String JSON) deriving (Eq, Ord)

instance Show JSON where
    show Null = "null"
    show (Object m) = "{" ++ L.intercalate "," (map (\ (x,y) -> show x ++ ":" ++ show y) $ M.toList m) ++ "}"
    show (Boolean True) = "true"
    show (Boolean False) = "false"
    show (String s) = show s
    show (Number n) = show n
    show (Array l) = show l

instance Read JSON where
  readsPrec _ x = parse x

parse :: ReadS (JSON)
parse json = [(fst $ parse' $ tokenize json, "")]

parseObject :: M.Map String JSON -> [Token] -> (JSON, [Token])
parseObject acc (TCloseBrace : xs) = (Object acc, xs)
parseObject acc (TComma : xs) = parseObject acc xs
parseObject acc (TString s : TColon : xs) =
  let (json, xss) = parse' xs
  in parseObject (M.insert s json acc) xss

parseArray :: [JSON] -> [Token] -> (JSON, [Token])
parseArray acc (TCloseBracket:xs) = (Array (reverse acc), xs)
parseArray acc (TComma:xs) = parseArray acc xs
parseArray acc object = 
  let (json, xs) = parse' object 
  in parseArray (json:acc) xs

parse' :: [Token] -> (JSON, [Token])
parse' (TOpenBrace:xs) = parseObject M.empty xs 
parse' (TOpenBracket:xs) = parseArray [] xs
parse' (TNull:xs) = (Null, xs)
parse' (TBoolean b:xs) = (Boolean b, xs)
parse' (TString st:xs) = (String st, xs)
parse' (TNumber num:xs) = (Number num, xs)
