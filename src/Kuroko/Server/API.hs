{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Server.API
  ( KurokoAPI
  , ReactRequest (..)
  , ReactResponse (..)
  , serveKurokoAPI
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Effectful
import Effectful.Servant.Server (Server, serve)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Servant.API

import Kuroko.Core.Types
import Kuroko.Effect.LLM
import Kuroko.Effect.Tool
import Kuroko.Workflow.ReAct

-- | Request payload for running an agent task.
data ReactRequest = ReactRequest
  { prompt   :: !Text
  , model    :: !(Maybe Text)
  , maxSteps :: !(Maybe Int)
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Response payload returning the conversation and final answer.
data ReactResponse = ReactResponse
  { finalAnswer :: !Text
  , totalTurns  :: !Int
  , history     :: ![Message]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Type-level specification of the Kuroko Agent REST API.
type KurokoAPI =
  "api" :> "v1" :>
    (    "react" :> ReqBody '[JSON] ReactRequest :> Post '[JSON] ReactResponse
    :<|> "tools" :> Get '[JSON] [ToolDef]
    )

-- | Servant-effectful implementation of Kuroko's REST API.
kurokoServer :: (IOE :> es, LLM :> es, ToolExec :> es) => Server KurokoAPI es
kurokoServer = handleReact :<|> listTools
  where
    handleReact req = do
      let targetModel = case req.model of
            Just m  -> ModelId m
            Nothing -> ModelId "gpt-4o"
          steps = case req.maxSteps of
            Just s  -> s
            Nothing -> 10
          initialHistory = [userMsg req.prompt]
      finalHistory <- runReActLoop steps targetModel initialHistory
      let answer = case reverse finalHistory of
            (lastMsg:_) -> lastMsg.content
            []          -> ""
      pure $ ReactResponse
        { finalAnswer = answer
        , totalTurns  = length finalHistory
        , history     = finalHistory
        }

-- | Turn the Kuroko Agent API into a WAI Application running in 'Eff es'.
serveKurokoAPI :: (IOE :> es, LLM :> es, ToolExec :> es) => Eff es Application
serveKurokoAPI = pure $ serve (Proxy @KurokoAPI) kurokoServer
