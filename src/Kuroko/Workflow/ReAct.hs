{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Workflow.ReAct
  ( runReActStep
  , runReActLoop
  ) where

import Control.Monad (forM)
import qualified Data.Text as T
import Effectful

import Kuroko.Core.Types
import Kuroko.Effect.LLM
import Kuroko.Effect.Policy
import Kuroko.Effect.Policy.Types
import Kuroko.Effect.Tool

-- | Execute a single ReAct step:
-- 1. Queries available tools via 'listTools'.
-- 2. Sends conversation to 'chatComplete'.
-- 3. Records token usage via 'recordUsage'.
-- 4. For each tool call, checks authorization with 'authorizeTool'.
-- 5. Executes allowed tools or returns policy denial/throttle diagnostics.
-- 6. Returns updated message list and a boolean flag ('True' = finished, 'False' = more tool work).
runReActStep
  :: (LLM :> es, ToolExec :> es, ToolPolicy :> es)
  => ModelId
  -> [Message]
  -> Eff es ([Message], Bool)
runReActStep modelId history = do
  defs <- listTools
  resp <- chatComplete modelId history defs
  recordUsage resp.usage
  let assistantResponse = resp.message
      historyWithAssistant = history ++ [assistantResponse]
  case assistantResponse.toolCalls of
    Nothing -> pure (historyWithAssistant, True)
    Just [] -> pure (historyWithAssistant, True)
    Just tcs -> do
      toolResultMsgs <- forM tcs $ \call -> do
        verdict <- authorizeTool call
        resultText <- case verdict of
          AllowAuto -> do
            res <- executeTool call
            pure $ case res of
              Right out -> out
              Left err  -> "Error: " <> err
          Throttled waitSecs ->
            pure $ "Rate limit exceeded: throttled for " <> T.pack (show waitSecs) <> "s"
          Denied reason ->
            pure $ "Policy Denied: " <> reason
          RequireConfirmation prompt ->
            pure $ "Confirmation Required: " <> prompt
        pure $ toolResultMsg call.toolCallId resultText
      pure (historyWithAssistant ++ toolResultMsgs, False)

-- | Run the ReAct loop up to a maximum step budget.
runReActLoop
  :: (LLM :> es, ToolExec :> es, ToolPolicy :> es)
  => Int       -- ^ Maximum steps allowed
  -> ModelId   -- ^ Target LLM model
  -> [Message] -- ^ Initial conversation history
  -> Eff es [Message]
runReActLoop maxSteps modelId initialHistory = go maxSteps initialHistory
  where
    go stepsLeft hist
      | stepsLeft <= 0 = pure hist
      | otherwise = do
          (nextHist, finished) <- runReActStep modelId hist
          if finished
            then pure nextHist
            else go (stepsLeft - 1) nextHist
