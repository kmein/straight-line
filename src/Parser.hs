{-# LANGUAGE LambdaCase #-}
module Parser (program, instruction, variable, expression) where

import Types

import Control.Applicative (Alternative(..))
import Numeric.Natural (Natural)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token (TokenParsing, natural)

program :: (TokenParsing p, Monad p) => p Program
program =
    try (Cons <$> (instruction <* sym ";") <*> program) <|>
    (singleton <$> (instruction <* eof)) <|>
    (Nil <$ eof)
  where
    singleton x = Cons x Nil

instruction :: (TokenParsing p, Monad p) => p Instruction
instruction = Assign <$> (variable <* sym ":=") <*> expression

variable :: (TokenParsing p, Monad p) => p Variable
variable =
    (Output <$ char 'o') <|> (Input <$> (char 'i' *> nat')) <|>
    (X <$> (char 'x' *> nat'))
  where
    nat' :: (CharParsing p, Monad p) => p Natural
    nat' =
        some digit >>=
        \case
            "0" -> pure 0
            ('0':xs) -> unexpected "leading zero"
            ds -> pure (read ds)

expression :: (TokenParsing p, Monad p) => p Expression
expression =
    try (Add <$> (variable <* sym "+") <*> variable) <|>
    (Multiply <$> (variable <* sym "*") <*> variable) <|>
    (Constant <$> nat)

nat :: TokenParsing p => p Natural
nat = fromIntegral <$> natural

sym :: CharParsing p => String -> p String
sym p = skipOptional spaces *> string p <* skipOptional spaces

