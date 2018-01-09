{-# LANGUAGE LambdaCase #-}
module Parser (program, instruction, variable, expression) where

import Types

import Control.Applicative (Alternative(..))
import Numeric.Natural (Natural)
import Text.Parser.Char
import Text.Parser.Combinators

program :: (CharParsing p, Monad p) => p Program
program =
    try (singleton <$> (instruction <* skipOptional spaces <* eof)) <|>
    (Cons <$> (instruction <* sym ";") <*> program) <|>
    (Nil <$ eof)
  where
    singleton x = Cons x Nil

instruction :: (CharParsing p, Monad p) => p Instruction
instruction = Assign <$> (variable <* sym ":=") <*> expression

variable :: (CharParsing p, Monad p) => p Variable
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

expression :: (CharParsing p, Monad p) => p Expression
expression =
    try (Add <$> (variable <* sym "+") <*> variable) <|>
    (Multiply <$> (variable <* sym "*") <*> variable) <|>
    (Constant <$> nat)

nat :: CharParsing p => p Natural
nat = read <$> some digit

sym :: CharParsing p => String -> p String
sym p = skipOptional spaces *> string p <* skipOptional spaces

