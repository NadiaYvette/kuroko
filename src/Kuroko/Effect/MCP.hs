{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Effect.MCP
  ( serveMCPStdio
  , kurokoToolsToEffTools
  , kurokoImplementation
  , kurokoServerCapabilities
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Effectful
import Effectful.MCP.Server
import MCP.Types

import Kuroko.Core.Types
import Kuroko.Effect.Tool

-- | Metadata describing the Kuroko MCP server implementation.
kurokoImplementation :: Implementation
kurokoImplementation = Implementation
  { name    = "kuroko"
  , version = "0.1.0.0"
  , title   = Just "Kuroko Agent Runtime"
  }

-- | Default server capabilities for Kuroko (exposing tools).
kurokoServerCapabilities :: ServerCapabilities
kurokoServerCapabilities = ServerCapabilities
  { logging      = Nothing
  , prompts      = Nothing
  , resources    = Nothing
  , tools        = Just (ToolsCapability (Just False))
  , completions  = Nothing
  , experimental = Nothing
  }

-- | Convert Kuroko 'ToolDef' entries into typed 'EffTool' runners.
kurokoToolsToEffTools :: (ToolExec :> es) => [ToolDef] -> [EffTool es]
kurokoToolsToEffTools defs = map convert defs
  where
    convert def = mkEffTool def.name (Just def.description) schema exec
      where
        schema = case Aeson.fromJSON def.parameters of
          Aeson.Success (s :: InputSchema) -> s
          Aeson.Error _                    -> InputSchema "object" Nothing Nothing

        exec mArgs = do
          let argsVal = case mArgs of
                Just m  -> Aeson.Object (KM.fromMap (Map.mapKeys K.fromText m))
                Nothing -> Aeson.Null
          res <- executeTool (ToolCall "mcp-call" def.name argsVal)
          pure $ case res of
            Right out -> toolTextResult [out]
            Left err  -> toolTextError err

-- | Serve Kuroko tools over stdio using typed Effectful MCP server infrastructure.
serveMCPStdio :: (IOE :> es, ToolExec :> es) => Eff es ()
serveMCPStdio = do
  defs <- listTools
  let tools = kurokoToolsToEffTools defs
  serveStdioEff kurokoImplementation kurokoServerCapabilities tools
