module Language.Hack.VM.Lexer where

import Text.Parsec
import Text.Parsec.String
    

symbol :: Parser String
symbol = do
  c <- letter <|> oneOf ".:_"
  cs <- many alphaNum
  return $ c:cs
