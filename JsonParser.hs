module JsonParser where

import           Control.Applicative (Alternative (..), some, (<|>))
import           Data.Char           (isDigit)
import qualified Data.Map.Strict     as M

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Int
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject (M.Map String JsonValue)
               deriving (Show)

newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
--  fmap f p = Parser $ \input -> do
--                         (input', x) <- runParser p input
--                         Just (input', f x)

    fmap f p = Parser $ fmap (fmap f) . runParser p

instance Applicative Parser where
  pure x    = Parser $ \input -> Just (input, x)
  p1 <*> p2 = Parser $ \input -> do
                         (input', f) <- runParser p1 input
                         (input'', x) <- runParser p2 input'
                         Just (input'', f x)

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \input -> maybe (runParser p2 input) pure (runParser p1 input)

charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x:xs) = if c == x then Just (xs, x) else Nothing
    f []     = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
-- spanP pred = Parser $ Just . uncurry (flip (,)) . span pred
spanP pred = Parser $ \input -> let (x, input') = span pred input in Just (input', x)

between :: Parser a -> Parser b -> Parser c -> Parser b
between betweenLeft p betweenRight = betweenLeft *> p <* betweenRight

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

whiteSpaceP :: Parser String
whiteSpaceP = many (charP ' ' <|> charP '\n')

notEmpty :: Parser [a] -> Parser [a]
notEmpty p = Parser $ \input -> do
                                  (input', x) <- runParser p input
                                  if null x then Nothing else Just (input', x)

stringLiteralP :: Parser String
stringLiteralP = between (charP '"') (spanP (/= '"')) (charP '"')

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpaceP

-- JSON parsers

jsonValueP :: Parser JsonValue
jsonValueP = lexeme $ jsonNullP <|> jsonBoolP <|> jsonNumberP <|> jsonArrayP <|> jsonStringP <|> jsonObjectP

jsonNullP :: Parser JsonValue
jsonNullP = JsonNull <$ stringP "null"

jsonNumberP :: Parser JsonValue
jsonNumberP = JsonNumber . read <$> notEmpty (spanP isDigit)

jsonBoolP :: Parser JsonValue
jsonBoolP = JsonBool . f <$> (stringP "true" <|> stringP "false")
  where
    f "true"  = True
    f "false" = False

jsonArrayP :: Parser JsonValue
jsonArrayP = JsonArray <$> between (lexeme (charP '['))
                                   (sepBy (lexeme (charP ',')) jsonValueP)
                                   (lexeme (charP ']'))

jsonStringP :: Parser JsonValue
jsonStringP = JsonString <$> stringLiteralP

jsonObjectP :: Parser JsonValue
jsonObjectP = JsonObject . M.fromList <$> between
                                            (lexeme (charP '{'))
                                              (sepBy
                                                (lexeme (charP ','))
                                                ((,) <$> lexeme stringLiteralP <* lexeme (charP ':') <*> jsonValueP))
                                            (lexeme (charP '}'))
