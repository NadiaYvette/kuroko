{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kuroko.Core.Types
  ( Role (..)
  , Message (..)
  , ToolCall (..)
  , ToolDef (..)
  , ModelId (..)
  , Usage (..)
  , LLMResponse (..)
  , userMsg
  , systemMsg
  , assistantMsg
  , toolResultMsg
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Conversation roles in LLM chat completions.
data Role
  = RoleUser
  | RoleSystem
  | RoleAssistant
  | RoleTool
  deriving (Eq, Show, Ord, Generic)

instance ToJSON Role where
  toJSON = \case
    RoleUser -> "user"
    RoleSystem -> "system"
    RoleAssistant -> "assistant"
    RoleTool -> "tool"

instance FromJSON Role where
  parseJSON = Aeson.withText "Role" $ \case
    "user" -> pure RoleUser
    "system" -> pure RoleSystem
    "assistant" -> pure RoleAssistant
    "tool" -> pure RoleTool
    other -> fail $ "Unknown role: " <> show other

-- | Model identifier (e.g. "gpt-4o", "claude-3-5-sonnet", "qwen2.5-coder").
newtype ModelId = ModelId { unModelId :: Text }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | A tool call emitted by an assistant response.
data ToolCall = ToolCall
  { toolCallId :: !Text
  , toolName   :: !Text
  , arguments  :: !Value
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | An exchange message in a conversation thread.
data Message = Message
  { role         :: !Role
  , content      :: !Text
  , toolCalls    :: !(Maybe [ToolCall])
  , toolCallId   :: !(Maybe Text)
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Smart constructor for user message.
userMsg :: Text -> Message
userMsg txt = Message RoleUser txt Nothing Nothing

-- | Smart constructor for system prompt.
systemMsg :: Text -> Message
systemMsg txt = Message RoleSystem txt Nothing Nothing

-- | Smart constructor for assistant reply.
assistantMsg :: Text -> Maybe [ToolCall] -> Message
assistantMsg txt tcs = Message RoleAssistant txt tcs Nothing

-- | Smart constructor for tool result return.
toolResultMsg :: Text -> Text -> Message
toolResultMsg callId result = Message RoleTool result Nothing (Just callId)

-- | Tool definition exposing metadata and JSON Schema for model selection.
data ToolDef = ToolDef
  { name        :: !Text
  , description :: !Text
  , parameters  :: !Value  -- ^ JSON Schema
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Token usage metadata for billing & budget caps.
data Usage = Usage
  { promptTokens     :: !Int
  , completionTokens :: !Int
  , totalTokens      :: !Int
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Structured response from an LLM invocation.
data LLMResponse = LLMResponse
  { message :: !Message
  , usage   :: !Usage
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)
