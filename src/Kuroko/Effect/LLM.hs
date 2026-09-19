{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Effect.LLM
  ( LLM (..)
  , chatComplete
  , streamTokens
  ) where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH

import Kuroko.Core.Types

-- | First-class algebraic effect for interacting with Language Models.
data LLM :: Effect where
  -- | Send a conversation thread and available tools, awaiting full response.
  ChatComplete :: ModelId -> [Message] -> [ToolDef] -> LLM m LLMResponse
  -- | Stream completion tokens incrementally to a consumer callback.
  StreamTokens :: ModelId -> [Message] -> (Text -> m ()) -> LLM m LLMResponse

makeEffect ''LLM
