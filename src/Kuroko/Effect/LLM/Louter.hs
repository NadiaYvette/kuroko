{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Effect.LLM.Louter
  ( runLLMLouter
  , runLLMLouterWithConfig
  , louterOpenAIConfig
  , louterAnthropicConfig
  , louterGeminiConfig
  , louterLlamaCppConfig
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Effectful hiding ((:>))
import qualified Effectful as Eff
import Effectful.Dispatch.Dynamic
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (BaseUrl (..), Scheme (..))

import Kuroko.Core.Types
import Kuroko.Effect.LLM

import qualified Louter.Client.Servant as LouterClient
import qualified Louter.Core.Error as LouterErr
import qualified Louter.Core.Types as LouterTypes

-- | Preset configuration helpers for Louter backends.
louterOpenAIConfig :: Text -> LouterClientConfig
louterOpenAIConfig = LouterClient.defaultOpenAIConfig

louterAnthropicConfig :: Text -> LouterClientConfig
louterAnthropicConfig = LouterClient.defaultAnthropicConfig

louterGeminiConfig :: Text -> LouterClientConfig
louterGeminiConfig = LouterClient.defaultGeminiConfig

louterLlamaCppConfig :: BaseUrl -> LouterClientConfig
louterLlamaCppConfig = LouterClient.defaultLlamaCppConfig

type LouterClientConfig = LouterClient.LouterClientConfig

-- | Interpret Kuroko's 'LLM' effect using Louter's typed multi-provider routing engine.
runLLMLouter :: (Eff.IOE Eff.:> es) => LouterClientConfig -> Eff (LLM : es) a -> Eff es a
runLLMLouter cfg action = do
  mgr <- liftIO $ newManager tlsManagerSettings
  interpret (\env -> \case
    ChatComplete mId msgs tools -> liftIO $ do
      let openAIReq = toOpenAIReq mId msgs tools
      res <- LouterClient.runLouterChat mgr cfg openAIReq
      case res of
        Left err -> error $ "Kuroko.Effect.LLM.Louter error: " <> T.unpack (LouterErr.formatLouterError err)
        Right openAIResp -> pure (fromOpenAIResp openAIResp)
    StreamTokens mId msgs callback -> do
      llmResp <- liftIO $ do
        let openAIReq = toOpenAIReq mId msgs []
        res <- LouterClient.runLouterChat mgr cfg openAIReq
        case res of
          Left err -> error $ "Kuroko.Effect.LLM.Louter stream error: " <> T.unpack (LouterErr.formatLouterError err)
          Right openAIResp -> pure (fromOpenAIResp openAIResp)
      localSeqUnlift env $ \unlift -> unlift (callback llmResp.message.content)
      pure llmResp
    ) action

-- | Interpret 'LLM' with an explicit Louter configuration and pre-existing HTTP manager.
runLLMLouterWithConfig
  :: (Eff.IOE Eff.:> es)
  => Manager
  -> LouterClientConfig
  -> Eff (LLM : es) a
  -> Eff es a
runLLMLouterWithConfig mgr cfg action =
  interpret (\env -> \case
    ChatComplete mId msgs tools -> liftIO $ do
      let openAIReq = toOpenAIReq mId msgs tools
      res <- LouterClient.runLouterChat mgr cfg openAIReq
      case res of
        Left err -> error $ "Kuroko.Effect.LLM.Louter error: " <> T.unpack (LouterErr.formatLouterError err)
        Right openAIResp -> pure (fromOpenAIResp openAIResp)
    StreamTokens mId msgs callback -> do
      llmResp <- liftIO $ do
        let openAIReq = toOpenAIReq mId msgs []
        res <- LouterClient.runLouterChat mgr cfg openAIReq
        case res of
          Left err -> error $ "Kuroko.Effect.LLM.Louter stream error: " <> T.unpack (LouterErr.formatLouterError err)
          Right openAIResp -> pure (fromOpenAIResp openAIResp)
      localSeqUnlift env $ \unlift -> unlift (callback llmResp.message.content)
      pure llmResp
    ) action

-- ============================================================================
-- Conversion Helpers
-- ============================================================================

toOpenAIReq :: ModelId -> [Message] -> [ToolDef] -> LouterTypes.OpenAIReq
toOpenAIReq mId msgs tools = LouterTypes.OpenAIReq
  { model       = mId.unModelId
  , messages    = map toOpenAIMsg msgs
  , tools       = if null tools then Nothing else Just (map toOpenAIToolDef tools)
  , temperature = Nothing
  , max_tokens  = Nothing
  , stream      = Nothing
  }
  where
    toOpenAIMsg :: Message -> LouterTypes.OpenAIMessage
    toOpenAIMsg m =
      let rText = case m.role of
            RoleUser      -> "user"
            RoleAssistant -> "assistant"
            RoleSystem    -> "system"
            RoleTool      -> "tool"
          tcs = case m.toolCalls of
            Just cs -> Just [ LouterTypes.OpenAIToolCall c.toolCallId "function" (LouterTypes.OpenAIFunction c.toolName (TE.decodeUtf8 (BSL.toStrict (Aeson.encode c.arguments)))) | c <- cs ]
            Nothing -> Nothing
      in LouterTypes.OpenAIMessage rText (Just m.content) tcs m.toolCallId

    toOpenAIToolDef :: ToolDef -> Value
    toOpenAIToolDef t = object
      [ "type" .= ("function" :: Text)
      , "function" .= object
          [ "name" .= t.name
          , "description" .= t.description
          , "parameters" .= t.parameters
          ]
      ]

fromOpenAIResp :: LouterTypes.OpenAIResp -> LLMResponse
fromOpenAIResp resp = case resp.choices of
  [] -> LLMResponse (Message RoleAssistant "" Nothing Nothing) (Usage 0 0 0)
  (c:_) ->
    let m = c.message
        r = case m.role of
          "user"      -> RoleUser
          "assistant" -> RoleAssistant
          "system"    -> RoleSystem
          "tool"      -> RoleTool
          _           -> RoleAssistant
        tcs = case m.tool_calls of
          Just cs -> Just [ ToolCall tc.id tc.function.name (parseArgs tc.function.arguments) | tc <- cs ]
          Nothing -> Nothing
        contentTxt = fromMaybe "" m.content
        usageObj = case resp.usage of
          Just u  -> Usage u.prompt_tokens u.completion_tokens u.total_tokens
          Nothing -> Usage 0 0 0
    in LLMResponse (Message r contentTxt tcs m.tool_call_id) usageObj
  where
    parseArgs raw = case Aeson.decode (BSL.fromStrict (TE.encodeUtf8 raw)) of
      Just val -> val
      Nothing  -> object []
