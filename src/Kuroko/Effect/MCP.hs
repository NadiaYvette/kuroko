{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Effect.MCP
  ( serveMCPStdio
  ) where

import Control.Monad (forever)
import Data.Aeson (Value, (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Effectful
import System.IO (hFlush, stdout)

import Kuroko.Core.Types
import Kuroko.Effect.Tool

-- | Serve tools over stdio as an MCP (Model Context Protocol) JSON-RPC 2.0 server.
serveMCPStdio :: (IOE :> es, ToolExec :> es) => Eff es ()
serveMCPStdio = forever $ do
  line <- liftIO TIO.getLine
  case Aeson.eitherDecode (BSL.fromStrict (TE.encodeUtf8 line)) of
    Left err -> liftIO $ do
      TIO.putStrLn $ "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,\"message\":" <> Text.pack (show err) <> "},\"id\":null}"
      hFlush stdout
    Right (reqObj :: Value) -> handleJsonRpc reqObj

handleJsonRpc :: (IOE :> es, ToolExec :> es) => Value -> Eff es ()
handleJsonRpc req = case req of
  Aeson.Object obj -> do
    let reqId = KM.lookup (K.fromText "id") obj
    case KM.lookup (K.fromText "method") obj of
      Just (Aeson.String "tools/list") -> do
        defs <- listTools
        let resp = object
              [ "jsonrpc" .= ("2.0" :: Text)
              , "id"      .= reqId
              , "result"  .= object ["tools" .= [ object ["name" .= d.name, "description" .= d.description, "inputSchema" .= d.parameters] | d <- defs ]]
              ]
        sendJson resp

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
                resp = object
                  [ "jsonrpc" .= ("2.0" :: Text)
                  , "id"      .= reqId
                  , "result"  .= object
                      [ "content" .= [ object ["type" .= ("text" :: Text), "text" .= textRes] ]
                      , "isError" .= isErr
                      ]
                  ]
            sendJson resp
          _ -> sendError reqId (-32602) "Invalid params for tools/call"

      Just (Aeson.String "initialize") -> do
        let resp = object
              [ "jsonrpc" .= ("2.0" :: Text)
              , "id"      .= reqId
              , "result"  .= object
                  [ "protocolVersion" .= ("2024-11-05" :: Text)
                  , "capabilities"    .= object ["tools" .= object []]
                  , "serverInfo"      .= object ["name" .= ("kuroko" :: Text), "version" .= ("0.1.0.0" :: Text)]
                  ]
              ]
        sendJson resp

      Just (Aeson.String m) -> sendError reqId (-32601) ("Method not found: " <> m)
      _ -> sendError reqId (-32600) "Invalid request"
  _ -> sendError Nothing (-32600) "Request must be an object"

sendJson :: IOE :> es => Value -> Eff es ()
sendJson val = liftIO $ do
  BSL.putStr (Aeson.encode val)
  putStrLn ""
  hFlush stdout

sendError :: IOE :> es => Maybe Value -> Int -> Text -> Eff es ()
sendError reqId code msg = do
  let resp = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id"      .= reqId
        , "error"   .= object ["code" .= code, "message" .= msg]
        ]
  sendJson resp
