a = "{\"abc\":true,\"def\":123,\"ghj\":{\"s\":61},\"ikl\":[1,2,3, \"123\"]}"

data Token = TOpenBrace 
  | TCloseBrace
  | TOpenBracket 
  | TCloseBracket
  | TColon 
  | TComma
  | TString String
  | TNumber Integer
  | TBoolean Bool
  | TNull deriving (Show)

tokenize :: String -> [Token]
tokenize string = 
  let
    tokenize' [] tokens = tokens
    tokenize' ('n':'u':'l':'l':xs) tokens = tokenize' xs (TNull:tokens)
  in tokenize' string []

tokenize "null"
