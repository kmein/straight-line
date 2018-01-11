{-# LANGUAGE LambdaCase #-}
module Parser (program, instruction, variable, expression) where

import Types

import GHC.Exts (fromList)
import Numeric.Natural (Natural)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

program :: (TokenParsing p, Monad p) => p Program
program = (Program . fromList) <$> semiSep1 instruction

instruction :: (TokenParsing p, Monad p) => p Instruction
instruction = Assign <$> (variable <* symbol ":=") <*> expression <?> "instruction"

variable :: (TokenParsing p, Monad p) => p Variable
variable =
    choice
        [ Output <$ symbolic 'o'
        , Input <$> token (char 'i' *> nat')
        , X <$> token (char 'x' *> nat')] <?> "variable"
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
    choice
        [ try $ Add <$> (variable <* symbolic '+') <*> variable
        , Multiply <$> (variable <* symbolic '*') <*> variable
        , (Constant . fromIntegral) <$> decimal] <?> "expression"
