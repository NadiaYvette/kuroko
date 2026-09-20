{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Server.MCP
  ( McpServantAPI
  , serveMcpHttp
  ) where

import Effectful
import Effectful.MCP.Servant (McpServantAPI, serveMcpApplication)
import Effectful.Wai (Application)

import Kuroko.Effect.MCP (kurokoImplementation, kurokoServerCapabilities, kurokoToolsToEffTools)
import Kuroko.Effect.Tool

-- | Turn Kuroko registered tools into an Effectful WAI MCP application endpoint.
serveMcpHttp :: (IOE :> es, ToolExec :> es) => Eff es (Application es)
serveMcpHttp = do
  defs <- listTools
  let tools = kurokoToolsToEffTools defs
  pure $ serveMcpApplication kurokoImplementation kurokoServerCapabilities tools
