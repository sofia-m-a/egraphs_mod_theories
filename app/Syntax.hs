module Syntax () where

import Data.List (List)
import Data.List.NonEmpty (some1)
import Lude
import Text.Megaparsec (MonadParsec (label), Parsec, oneOf)
import Text.Megaparsec.Char (digitChar, letterChar, space1)
import Text.Megaparsec.Char.Lexer (indentLevel)
import Text.Megaparsec.Char.Lexer qualified as L

data Program = ProgramC [Item]

data Item = ItemC ItemKind STerm STerm [Item]

data ItemKind
  = Signature
  | Definition
  | Semidefinition

newtype Name = NameC Text

data STerm
  = VarT Name
  | ApplyT (NonEmpty STerm)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser ()
symbol = void . L.symbol spaceConsumer

name :: Parser Name
name = NameC <$> lexeme ((fromString .) . (:) <$> letterChar <*> many gen)
  where
    gen = label "character in identifier" $ letterChar <|> digitChar <|> oneOf @List "'"

operator :: Parser Name
operator =
  NameC
    <$> lexeme
      ( choice @List
          [ (\c d s -> fromString (c : d : s)) <$> reserved1 <*> (reserved1 <|> gen) <*> many (reserved1 <|> gen),
            (\c s -> fromString (c : s)) <$> gen <*> many (reserved1 <|> gen)
          ]
      )
  where
    reserved1 = oneOf @List "←=:"
    gen = label "character in operator" $ oneOf @List "→`~!@$%^&*-_=+;\\|,.<>/?"

parseSTerm :: Parser STerm
parseSTerm = label "term" $ ApplyT <$> some1 stype0
  where
    stype0 :: Parser STerm
    stype0 =
      choice @List
        [ VarT <$> name,
          symbol "(" *> parseSTerm <* symbol ")"
        ]

-- parseItem :: Parser Item
-- parseItem =
--   _
--     <$> parseSTerm
--     <*> choice @List
--       [ Signature <$ symbol ":",
--         Definition <$ symbol "=",
--         Semidefinition <$ symbol "←"
--       ]
--     <*> choice @List
--       [ _ <$> symbol "let" <*> do
--           l <- indentLevel
--           indentGuard spaceConsumer GE
--         _ <$> parseSTerm
--       ]