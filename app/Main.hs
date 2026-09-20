{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful
import Options.Applicative

import Kuroko

data Command
  = CmdRun RunOpts
  | CmdMCP

data RunOpts = RunOpts
  { optModel  :: !String
  , optConfig :: !(Maybe FilePath)
  , optPrompt :: !String
  }

parseCommand :: Parser Command
parseCommand = subparser
  (  command "run" (info (CmdRun <$> parseRunOpts) (progDesc "Run Kuroko ReAct loop on a prompt"))
  <> command "mcp" (info (pure CmdMCP) (progDesc "Run Kuroko as a stdio Model Context Protocol (MCP) server"))
  )

parseRunOpts :: Parser RunOpts
parseRunOpts = RunOpts
  <$> strOption
      (  long "model"
      <> short 'm'
      <> value "gpt-4o"
      <> showDefault
      <> help "Target LLM model"
      )
  <*> optional (strOption
      (  long "config"
      <> short 'c'
      <> help "Path to agent.dhall configuration"
      ))
  <*> strArgument
      (  metavar "PROMPT"
      <> help "Initial user prompt"
      )

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    CmdMCP -> runEff $ do
      let reg = emptyRegistry
      runToolExecRegistry reg serveMCPStdio
    CmdRun opts' -> do
      cfg <- case opts'.optConfig of
        Just p  -> loadAgentConfig p
        Nothing -> pure defaultAgentConfig
      TIO.putStrLn $ "=== Kuroko Agent [" <> cfg.name <> "] ==="
      runEff $ do
        -- Mock runner demonstration for initial scaffold
        let mockResponse = LLMResponse
              { message = assistantMsg ("Acknowledged: " <> T.pack opts'.optPrompt <> "\nReady for instructions.") Nothing
              , usage = Usage 10 20 30
              }
            reg = emptyRegistry
            initialHistory =
              [ systemMsg cfg.systemPrompt
              , userMsg (T.pack opts'.optPrompt)
              ]
        history <- runLLMConstant mockResponse $
          runToolExecRegistry reg $
            runReActLoop 5 (ModelId (T.pack opts'.optModel)) initialHistory
        liftIO $ forM_ history $ \msg -> do
          TIO.putStrLn $ "[" <> T.pack (show msg.role) <> "]: " <> msg.content
  where
    forM_ = flip mapM_
    opts = info (parseCommand <**> helper)
      (  fullDesc
      <> progDesc "Kuroko (黒子) — Autonomous Agent Sidecar in Modern Haskell"
      <> header "kuroko - High-assurance, effectful AI coding assistant"
      )
