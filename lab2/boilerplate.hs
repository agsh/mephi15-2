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

{----------------------------------------}
{-# LANGUAGE OverloadedStrings #-}

-- https://github.com/JakeWheat/intro_to_parsing

import Control.Applicative ((<$>), (<*>), (*>), (<*), (<$))
import qualified Data.Map as M
import qualified Data.List as L
import Text.ParserCombinators.Parsec
import Text.Parsec.Token

data JSON = Null
            | Boolean Bool
            | String String
            | Number Integer
            | Array [JSON]
            | Object (M.Map String JSON) deriving (Eq, Ord)

instance Show JSON where
    show Null = "null"
    show (Object m) = "{" ++ L.intercalate "," (map (\ (x,y) -> show x ++ ":" ++ show y) $ M.toList m) ++ "}"
    show (Boolean b) = show b
    show (String s) = show s
    show (Number n) = show n
    show (Array l) = show l

readExpr :: String -> JSON
readExpr input = case parse parseExpr "JSON" input of
    Right val -> val
{-
parse
  :: Text.Parsec.Prim.Stream s Data.Functor.Identity.Identity t =>
     Text.Parsec.Prim.Parsec s () a
     -> SourceName -> s -> Either ParseError a
-}

parseString :: Parser JSON
parseString = char '\"' *> (String <$> many (noneOf "\"")) <* char '\"'

parseNumber :: Parser JSON
parseNumber = Number . read <$> many1 digit

parseList :: Parser JSON
parseList = char '[' *> spaces *> (Array <$> sepBy parseExpr (char ',' <* spaces)) <* spaces <* char ']'

parseBool :: Parser JSON
parseBool = Boolean True <$ string "true" <|> 
            Boolean False <$ string "false"

parseNull :: Parser JSON
parseNull = Null <$ string "null"

parseObject :: Parser (M.Map String JSON)
parseObject = char '{' *> (M.fromList <$> sepBy (parseContent <* spaces) (char ',' <* spaces)) <* char '}'
                where 
                    parseStr :: Parser String
                    parseStr = char '\"' *> (many (noneOf "\"")) <* char '\"'
                    parseContent = (,) <$> (parseStr <* char ':' <* spaces) <*> parseExpr

parseExpr = parseString
         <|> parseNumber
         <|> parseList
         <|> parseBool
         <|> parseNull
         <|> (Object <$> parseObject)
         <?> "Wrong JSON format"

a = "{\"123\":true,\"2394\":123,\"918\":61,\"81130\":[1,2,3, \"123\"]}"
