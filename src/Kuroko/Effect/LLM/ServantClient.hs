{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Effect.LLM.ServantClient
  ( ChatCompletionsAPI
  , ChatReq (..)
  , ChatResp (..)
  , ChatChoice (..)
  , runLLMServantClient
  ) where

import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Effectful hiding ((:>))
import qualified Effectful as Eff
import Effectful.Dispatch.Dynamic
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

import Kuroko.Core.Types
import Kuroko.Effect.LLM

-- | Compact request payload for Chat Completions.
data ChatReq = ChatReq
  { model    :: !Text
  , messages :: ![Message]
  , tools    :: ![Value]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Choice entry in response.
data ChatChoice = ChatChoice
  { index        :: !Int
  , message      :: !Message
  , finishReason :: !(Maybe Text)
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Minimal response payload from Chat Completions.
data ChatResp = ChatResp
  { id      :: !Text
  , choices :: ![ChatChoice]
  , usage   :: !(Maybe Usage)
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Type-level Servant specification of the Chat Completions endpoint.
-- Completely replaces thousands of lines of OpenAPI generated boilerplate.
type ChatCompletionsAPI =
  "v1" :> "chat" :> "completions"
       :> Header "Authorization" Text
       :> ReqBody '[JSON] ChatReq
       :> Post '[JSON] ChatResp

chatCompletionsClient :: Maybe Text -> ChatReq -> ClientM ChatResp
chatCompletionsClient = client (Proxy @ChatCompletionsAPI)

-- | Interpret the 'LLM' effect using Servant-Client.
runLLMServantClient :: (Eff.IOE Eff.:> es) => BaseUrl -> Text -> Eff (LLM : es) a -> Eff es a
runLLMServantClient baseUrl apiKey action = do
  mgr <- liftIO $ newManager tlsManagerSettings
  let clientEnv = mkClientEnv mgr baseUrl
      authHeader = Just ("Bearer " <> apiKey)
      doChat (ModelId mName) msgs toolDefs = do
        let toolObjs = [ object ["type" .= ("function" :: Text), "function" .= td] | td <- toolDefs ]
            reqPayload = ChatReq mName msgs toolObjs
        runClientM (chatCompletionsClient authHeader reqPayload) clientEnv >>= \case
          Left clientErr -> error $ "Servant client error calling LLM: " <> show clientErr
          Right resp -> case resp.choices of
            [] -> error "Servant client error: LLM returned empty choices list"
            (firstChoice:_) -> do
              let finalUsage = case resp.usage of
                    Just u  -> u
                    Nothing -> Usage 0 0 0
              pure $ LLMResponse firstChoice.message finalUsage

  interpret (\env -> \case
    ChatComplete mid msgs toolDefs -> liftIO $ doChat mid msgs toolDefs
    StreamTokens mid msgs callback -> do
      res <- liftIO $ doChat mid msgs []
      localSeqUnlift env $ \unlift -> unlift (callback res.message.content)
      pure res
    ) action
