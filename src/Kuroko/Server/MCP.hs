{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Server.MCP
  ( McpHttpAPI
  , serveMcpHttp
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful
import Effectful.Servant.Server (Server, serve)
import Network.Wai (Application)
import Servant.API

import Kuroko.Core.Types
import Kuroko.Effect.Tool

-- | Type-level specification of the MCP HTTP/SSE transport endpoint.
type McpHttpAPI =
       "sse"     :> Get '[PlainText] Text
  :<|> "message" :> QueryParam "sessionId" Text
                 :> ReqBody '[JSON] Value
                 :> Post '[JSON] Value

-- | Servant-effectful implementation of the MCP HTTP endpoint.
-- Runs directly inside Kuroko's effect stack without unlifting!
mcpServer :: (IOE :> es, ToolExec :> es) => Server McpHttpAPI es
mcpServer = handleSse :<|> handleMessage
  where
    handleSse = pure "MCP SSE Endpoint Ready\n"

    handleMessage _mSid req = case req of
      Aeson.Object obj -> do
        let reqId = KM.lookup (K.fromText "id") obj
        case KM.lookup (K.fromText "method") obj of
          Just (Aeson.String "tools/list") -> do
            defs <- listTools
            pure $ object
              [ "jsonrpc" .= ("2.0" :: Text)
              , "id"      .= reqId
              , "result"  .= object ["tools" .= [ object ["name" .= d.name, "description" .= d.description, "inputSchema" .= d.parameters] | d <- defs ]]
              ]

          Just (Aeson.String "tools/call") -> do
            case KM.lookup (K.fromText "params") obj of
              Just (Aeson.Object pObj) -> do
                let tName = case KM.lookup (K.fromText "name") pObj of
                      Just (Aeson.String n) -> n
                      _                     -> ""
                    tArgs = case KM.lookup (K.fromText "arguments") pObj of
                      Just v  -> v
                      Nothing -> Aeson.Null
                res <- executeTool (ToolCall "mcp-call" tName tArgs)
                let (isErr, textRes) = case res of
                      Right out -> (False, out)
                      Left err  -> (True, err)
                pure $ object
                  [ "jsonrpc" .= ("2.0" :: Text)
                  , "id"      .= reqId
                  , "result"  .= object
                      [ "content" .= [ object ["type" .= ("text" :: Text), "text" .= textRes] ]
                      , "isError" .= isErr
                      ]
                  ]
              _ -> pure $ jsonRpcError reqId (-32602) "Invalid params for tools/call"

          Just (Aeson.String "initialize") -> do
            pure $ object
              [ "jsonrpc" .= ("2.0" :: Text)
              , "id"      .= reqId
              , "result"  .= object
                  [ "protocolVersion" .= ("2024-11-05" :: Text)
                  , "capabilities"    .= object ["tools" .= object []]
                  , "serverInfo"      .= object ["name" .= ("kuroko" :: Text), "version" .= ("0.1.0.0" :: Text)]
                  ]
              ]

          Just (Aeson.String m) -> pure $ jsonRpcError reqId (-32601) ("Method not found: " <> m)
          _ -> pure $ jsonRpcError reqId (-32600) "Invalid request"
      _ -> pure $ jsonRpcError Nothing (-32600) "Request must be an object"

    jsonRpcError reqId code msg = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id"      .= reqId
      , "error"   .= object ["code" .= code, "message" .= msg]
      ]

-- | Turn the MCP Servant API into a WAI Application running in 'Eff es'.
serveMcpHttp :: (IOE :> es, ToolExec :> es) => Eff es Application
serveMcpHttp = pure $ serve (Proxy @McpHttpAPI) mcpServer
