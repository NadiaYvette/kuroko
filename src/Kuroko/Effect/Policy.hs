{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Effect.Policy
  ( -- * Effect & Operations
    ToolPolicy (..)
  , authorizeTool
  , recordUsage
  , getPolicyState

    -- * State Types
  , PolicyState (..)
  , emptyPolicyState

    -- * Interpreters
  , runToolPolicyAllowAll
  , runToolPolicyMock
  , runToolPolicyBudgetAndRate
  , runToolPolicyConfig
  ) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Kuroko.Core.Types (ToolCall, Usage (..))
import Kuroko.Effect.Policy.Types

-- | Running tracking state for the active policy session.
data PolicyState = PolicyState
  { accumulatedTokens :: !Natural
  , accumulatedCalls  :: !Natural
  , callTimestamps    :: ![UTCTime]
  } deriving (Eq, Show, Generic)

-- | Empty initial policy state.
emptyPolicyState :: PolicyState
emptyPolicyState = PolicyState 0 0 []

-- | Dynamic effect for authorising tool invocations and recording resource usage.
data ToolPolicy :: Effect where
  AuthorizeTool  :: ToolCall -> ToolPolicy m PolicyVerdict
  RecordUsage    :: Usage    -> ToolPolicy m ()
  GetPolicyState :: ToolPolicy m PolicyState

makeEffect ''ToolPolicy

-- | Interpreter that permits all tool calls unconditionally and ignores usage.
runToolPolicyAllowAll :: Eff (ToolPolicy : es) a -> Eff es a
runToolPolicyAllowAll = interpret $ \_ -> \case
  AuthorizeTool _ -> pure AllowAuto
  RecordUsage _   -> pure ()
  GetPolicyState  -> pure emptyPolicyState

-- | Mock interpreter using a custom verdict predicate.
runToolPolicyMock
  :: (ToolCall -> PolicyVerdict)
  -> Eff (ToolPolicy : es) a
  -> Eff es a
runToolPolicyMock verdictFn = interpret $ \_ -> \case
  AuthorizeTool call -> pure $ verdictFn call
  RecordUsage _      -> pure ()
  GetPolicyState     -> pure emptyPolicyState

-- | Interpret 'ToolPolicy' with live budget and sliding-window rate tracking.
runToolPolicyBudgetAndRate
  :: IOE :> es
  => BudgetCap
  -> Maybe RateCap
  -> Eff (ToolPolicy : es) a
  -> Eff es a
runToolPolicyBudgetAndRate bCap maybeRateCap action = do
  stateRef <- liftIO $ newIORef emptyPolicyState
  interpret (\_ -> \case
    AuthorizeTool _call -> liftIO $ do
      now <- getCurrentTime
      st <- readIORef stateRef

      -- 1. Check cumulative token budget
      let tokenExceeded = case bCap.maxTotalTokens of
            Just maxToks | st.accumulatedTokens >= maxToks -> True
            _ -> False

      -- 2. Check cumulative tool call budget
      let callsExceeded = case bCap.maxToolCalls of
            Just maxCalls | st.accumulatedCalls >= maxCalls -> True
            _ -> False

      if tokenExceeded
        then pure $ Denied "Session total token budget exceeded."
        else if callsExceeded
          then pure $ Denied "Session tool call budget exceeded."
          else case maybeRateCap of
            Nothing -> do
              writeIORef stateRef st { accumulatedCalls = st.accumulatedCalls + 1 }
              pure AllowAuto
            Just rCap -> do
              let cutoff = addUTCTime (- realToFrac rCap.windowSeconds) now
                  recent = filter (>= cutoff) st.callTimestamps
              if fromIntegral (length recent) >= rCap.maxCallsPerWindow
                then do
                  let oldestInWindow = minimum recent
                      timeElapsed = realToFrac (diffUTCTime now oldestInWindow)
                      throttleSecs = max 0.001 (rCap.windowSeconds - timeElapsed)
                  pure $ Throttled throttleSecs
                else do
                  writeIORef stateRef st
                    { accumulatedCalls = st.accumulatedCalls + 1
                    , callTimestamps   = now : recent
                    }
                  pure AllowAuto

    RecordUsage u -> liftIO $ do
      st <- readIORef stateRef
      let additional = fromIntegral (max 0 u.totalTokens)
      writeIORef stateRef st { accumulatedTokens = st.accumulatedTokens + additional }

    GetPolicyState -> liftIO $ readIORef stateRef
    ) action

-- | Interpret 'ToolPolicy' using a top-level 'PolicyConfig'.
runToolPolicyConfig
  :: IOE :> es
  => PolicyConfig
  -> Eff (ToolPolicy : es) a
  -> Eff es a
runToolPolicyConfig cfg = runToolPolicyBudgetAndRate cfg.budgetCap cfg.rateCap
