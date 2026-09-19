{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kuroko.Config.Dhall
  ( AgentConfig (..)
  , defaultAgentConfig
  , loadAgentConfig
  ) where

import Data.Text (Text)
import qualified Dhall
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- | Typed agent configuration, validated and loaded via Dhall.
data AgentConfig = AgentConfig
  { name         :: !Text
  , model        :: !Text
  , systemPrompt :: !Text
  , maxTokens    :: !Natural
  , temperature  :: !Double
  } deriving (Eq, Show, Generic, Dhall.FromDhall, Dhall.ToDhall)

-- | Default agent configuration.
defaultAgentConfig :: AgentConfig
defaultAgentConfig = AgentConfig
  { name         = "Kuroko"
  , model        = "gpt-4o"
  , systemPrompt = "You are Kuroko (黒子), an unobtrusive, rigorous AI coding assistant."
  , maxTokens    = 4096
  , temperature  = 0.2
  }

-- | Load and typecheck an agent configuration from a Dhall file.
loadAgentConfig :: FilePath -> IO AgentConfig
loadAgentConfig path = Dhall.inputFile Dhall.auto path
