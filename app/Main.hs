{-# LANGUAGE LambdaCase #-}
module Main where

import Types (Program)
import qualified Parser
import qualified Evaluator

import Data.Monoid ((<>))
import Numeric.Natural (Natural)
import Options.Applicative
import Test.QuickCheck (generate, arbitrary)
import Text.Parsec (parse)
import Text.PrettyPrint.Leijen (pretty)

type Source = Either String FilePath

data Action
    = Execute [Natural] Source
    | PrettyPrint Bool Source
    | CompileTo FilePath Source
    | Generate

straightLineOptions :: Parser Action
straightLineOptions =
    hsubparser $
    command
        "exec"
        (info
             (flip Execute <$> parseSource <*>
              some -- at least one: ℕ^k
                  (argument
                       auto
                       (metavar "INPUT" <>
                        help "Natural numbers, input for the program")))
             (progDesc "Execute a program")) <>
    command
        "pretty"
        (info
             (PrettyPrint <$>
              switch
                  (long "internal" <> short 'i' <>
                   help "Display the internal representation") <*>
              parseSource)
             (progDesc "Pretty-print a program")) <>
    command
        "compile"
        (info
             (CompileTo <$>
              strOption
                  (short 'o' <> long "output" <> metavar "PATH" <>
                   help "The destination of the compiled program") <*>
              parseSource)
             (progDesc "Compile a program to machine code")) <>
    command "gen" (info (pure Generate) (progDesc "Generate a random program"))
  where
    parseSource =
        fmap
            Left
            (strOption
                 (short 'e' <> metavar "PROGRAM" <>
                  help "A Straight-Line program passed on the command line")) <|>
        fmap
            Right
            (strArgument
                 (metavar "PATH" <>
                  help "The input file containing the source code"))

mainWith :: Action -> IO ()
mainWith =
    \case
        Execute input source ->
            print . Evaluator.run input =<< readProgram source
        PrettyPrint internal source -> do
            prog <- readProgram source
            if internal
                then print prog
                else print $ pretty prog
        CompileTo _ _ -> error "Compilation not yet implemented."
        Generate -> print . pretty =<< (generate arbitrary :: IO Program)
  where
    readProgram source = do
        code <- either return readFile source
        let sourceName = either (const "(expr)") id source
            parseResult = parse Parser.program sourceName code
        return $ either (error . show) id parseResult

main :: IO ()
main = mainWith =<< execParser opts
    where opts = info (straightLineOptions <**> helper) fullDesc
