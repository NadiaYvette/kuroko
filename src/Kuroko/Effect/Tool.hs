{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Effect.Tool
  ( ToolExec (..)
  , executeTool
  , listTools
  , ToolHandler
  , ToolRegistry (..)
  , emptyRegistry
  , registerTool
  , runToolExecRegistry
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH

import Kuroko.Core.Types

-- | Effect for executing named tools and inspecting available definitions.
data ToolExec :: Effect where
  ExecuteTool :: ToolCall -> ToolExec m (Either Text Text)
  ListTools   :: ToolExec m [ToolDef]

makeEffect ''ToolExec

-- | Handler computation that executes a tool call given its parsed JSON arguments.
type ToolHandler es = ToolCall -> Eff es (Either Text Text)

-- | In-memory registry of tools mapped to their definitions and execution handlers.
data ToolRegistry es = ToolRegistry
  { regDefs     :: ![ToolDef]
  , regHandlers :: !(Map Text (ToolHandler es))
  }

-- | Empty tool registry.
emptyRegistry :: ToolRegistry es
emptyRegistry = ToolRegistry [] Map.empty

-- | Register a new tool definition and handler.
registerTool :: ToolDef -> ToolHandler es -> ToolRegistry es -> ToolRegistry es
registerTool def handler reg =
  ToolRegistry
    { regDefs = def : reg.regDefs
    , regHandlers = Map.insert def.name handler reg.regHandlers
    }

-- | Interpret 'ToolExec' given a concrete 'ToolRegistry'.
runToolExecRegistry :: ToolRegistry es -> Eff (ToolExec : es) a -> Eff es a
runToolExecRegistry reg action = interpret action $ \_ -> \case
  ExecuteTool call -> case Map.lookup call.toolName reg.regHandlers of
    Just handler -> handler call
    Nothing      -> pure $ Left $ "Unknown tool: " <> call.toolName
  ListTools -> pure reg.regDefs
