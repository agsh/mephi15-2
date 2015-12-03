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
