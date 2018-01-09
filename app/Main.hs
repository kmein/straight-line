module Main where

import qualified Parser
import qualified Evaluator

import Data.Monoid ((<>))
import Numeric.Natural (Natural)
import Options.Applicative
import Text.Parsec (parse)
import Text.PrettyPrint.Leijen (pretty)

data Action
    = Execute [Natural]
    | PrettyPrint Bool
    | CompileTo FilePath

straightLineOptions :: Parser (Action, Either String FilePath)
straightLineOptions =
    (,) <$>
    subparser
        (command
             "exec"
             (info
                  (Execute <$>
                   some
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
                       (long "in-place" <> short 'i' <>
                        help "Overwrite the source file"))
                  (progDesc "Pretty-print a program")) <>
         command
             "compile"
             (info
                  (CompileTo <$>
                   strOption
                       (short 'o' <> long "output" <> metavar "PATH" <>
                        help "The destination of the compiled program"))
                  (progDesc "Compile a program to machine code"))) <*>
    (fmap
         Left
         (strOption
              (short 'e' <> metavar "PROGRAM" <>
               help "A Straight-Line program passed on the command line")) <|>
     fmap
         Right
         (strArgument
              (metavar "PATH" <>
               help "The input file containing the source code")))

mainWith :: (Action, Either String FilePath) -> IO ()
mainWith (action, source) = do
    code <- either return readFile source
    let sourceName = either (const "(expr)") id source
        parseResult = parse Parser.program sourceName code
        prog = either (error . show) id parseResult
    case action of
        Execute input -> print $ Evaluator.run input prog
        PrettyPrint overwrite ->
            let prettified = show $ pretty prog
            in case source of
                   Right path
                       | overwrite -> writeFile path prettified
                   _ -> putStrLn prettified
        CompileTo _ -> error "Compilation not yet implemented."

main :: IO ()
main = mainWith =<< execParser opts
    where opts = info (straightLineOptions <**> helper) fullDesc
