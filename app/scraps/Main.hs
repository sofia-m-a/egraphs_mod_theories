module Main where

import Options.Applicative
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Lude

data BuildOptions = BuildOptions
  deriving (Eq, Show)

data Command
  = Listen {lineBased :: Bool}
  | Serve ServeCommand
  | Check BuildOptions
  | Build BuildOptions
  | Run BuildOptions
  deriving (Eq, Show)

data ServeCommand
  = Watch
  | LSP
  deriving (Eq, Show)

parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "listen" (info (Listen <$> parseListen) (progDesc "start a language listener"))
        <> command "serve" (info (Serve <$> parseServe) (progDesc "start a language server"))
    )

parseListen :: Parser Bool
parseListen = switch (long "line-based" <> short 'l' <> help "run a line-based listener rather than a rich terminal-based listener")

parseServe :: Parser ServeCommand
parseServe =
  subparser
    ( command "watch" (info (pure Watch) (progDesc "watch and rebuild on changes"))
        <> command "lsp" (info (pure LSP) (progDesc "serve according to the Language Server Protocol"))
    )

parser :: ParserInfo Command
parser =
  info (parseCommand <**> helper) $
    fullDesc
      <> failureCode 64 -- from OpenBSD sysexits(3)

main :: IO ()
main = do
  cmd <- execParser parser
  case cmd of
    Listen b -> print cmd >> putStrLn "unimplemented"
    Serve sc -> print cmd >> putStrLn "unimplemented"
    Check bo -> print cmd >> putStrLn "unimplemented"
    Build bo -> print cmd >> putStrLn "unimplemented"
    Run bo -> print cmd >> putStrLn "unimplemented"
