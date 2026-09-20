{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Effect.LLM.OpenAI
  ( OpenAIConfig (..)
  , defaultOpenAIConfig
  , runLLMOpenAI
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Foldable
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Effectful
import Effectful.Dispatch.Dynamic
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)

import Kuroko.Core.Types
import Kuroko.Effect.LLM

-- | Configuration parameters for calling OpenAI-compatible APIs.
data OpenAIConfig = OpenAIConfig
  { apiBaseUrl :: !Text
  , apiKey     :: !Text
  } deriving (Eq, Show)

-- | Default configuration pointing to standard OpenAI endpoint.
defaultOpenAIConfig :: Text -> OpenAIConfig
defaultOpenAIConfig key = OpenAIConfig
  { apiBaseUrl = "https://api.openai.com/v1"
  , apiKey     = key
  }

-- | Run the 'LLM' effect against an OpenAI-compatible endpoint.
runLLMOpenAI :: IOE :> es => OpenAIConfig -> Eff (LLM : es) a -> Eff es a
runLLMOpenAI cfg action = do
  manager <- liftIO newTlsManager
  let doChat (ModelId mName) msgs toolDefs = do
        let endpoint = Text.unpack cfg.apiBaseUrl <> "/chat/completions"
        initReq <- parseRequest endpoint
        let bodyPayload = object
              [ "model" .= mName
              , "messages" .= msgs
              , "tools" .= [ object ["type" .= ("function" :: Text), "function" .= td] | td <- toolDefs ]
              ]
            req = initReq
              { method = "POST"
              , requestHeaders =
                  [ ("Content-Type", "application/json")
                  , ("Authorization", "Bearer " <> TE.encodeUtf8 cfg.apiKey)
                  ]
              , requestBody = RequestBodyLBS (Aeson.encode bodyPayload)
              }
        resp <- httpLbs req manager
        case Aeson.eitherDecode (responseBody resp) of
          Right (val :: Value) -> case parseOpenAIResponse val of
            Right llmResp -> pure llmResp
            Left err -> error $ "Failed to parse OpenAI response: " <> err <> "\nBody: " <> show (responseBody resp)
          Left err -> error $ "Failed to decode HTTP response: " <> err <> "\nBody: " <> show (responseBody resp)

  interpret (\env -> \case
    ChatComplete mid msgs toolDefs -> liftIO $ doChat mid msgs toolDefs
    StreamTokens mid msgs callback -> do
      res <- liftIO $ doChat mid msgs []
      localSeqUnlift env $ \unlift -> unlift (callback res.message.content)
      pure res
    ) action

parseOpenAIResponse :: Value -> Either String LLMResponse
parseOpenAIResponse val = flip parseEither val $ Aeson.withObject "OpenAIResponse" $ \obj -> do
  choices <- obj Aeson..: "choices"
  case choices of
    [] -> fail "No choices returned in completion"
    (firstChoice:_) -> flip (Aeson.withObject "Choice") firstChoice $ \cObj -> do
      msgObj <- cObj Aeson..: "message"
      contentTxt <- msgObj Aeson..:? "content" Aeson..!= ""
      rawToolCalls <- msgObj Aeson..:? "tool_calls"
      parsedToolCalls <- case rawToolCalls of
        Nothing -> pure Nothing
        Just tcs -> fmap Just $ flip (Aeson.withArray "ToolCalls") tcs $ \arr ->
          mapM parseToolCall (Data.Foldable.toList arr)
      usageObj <- obj Aeson..:? "usage"
      parsedUsage <- case usageObj of
        Nothing -> pure (Usage 0 0 0)
        Just u -> flip (Aeson.withObject "Usage") u $ \uObj -> do
          p <- uObj Aeson..:? "prompt_tokens" Aeson..!= 0
          c <- uObj Aeson..:? "completion_tokens" Aeson..!= 0
          t <- uObj Aeson..:? "total_tokens" Aeson..!= 0
          pure (Usage p c t)
      pure $ LLMResponse (assistantMsg contentTxt parsedToolCalls) parsedUsage
  where
    parseToolCall = Aeson.withObject "ToolCall" $ \tObj -> do
      cid <- tObj Aeson..: "id"
      fnObj <- tObj Aeson..: "function"
      name <- fnObj Aeson..: "name"
      argsStr <- fnObj Aeson..: "arguments"
      let argsVal = case Aeson.decodeStrict (TE.encodeUtf8 argsStr) of
            Just v  -> v
            Nothing -> Aeson.String argsStr
      pure $ ToolCall cid name argsVal
