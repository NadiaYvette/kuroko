{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Kuroko.Effect.LLM.Mock
  ( runLLMMock
  , runLLMConstant
  ) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Effectful
import Effectful.Dispatch.Dynamic

import Kuroko.Core.Types
import Kuroko.Effect.LLM

-- | Interpret the 'LLM' effect against a fixed list of mock responses.
-- Pops responses sequentially; throws an error if responses are exhausted.
runLLMMock :: IOE :> es => [LLMResponse] -> Eff (LLM : es) a -> Eff es a
runLLMMock responses action = do
  ref <- liftIO $ newIORef responses
  interpret (\env -> \case
    ChatComplete _ _ _ -> liftIO $ do
      readIORef ref >>= \case
        [] -> error "runLLMMock: Mock responses exhausted!"
        (r:rs) -> do
          writeIORef ref rs
          pure r
    StreamTokens _ _ callback -> do
      m <- liftIO $ do
        readIORef ref >>= \case
          [] -> error "runLLMMock: Mock responses exhausted!"
          (r:rs) -> do
            writeIORef ref rs
            pure r
      localSeqUnlift env $ \unlift -> unlift (callback m.message.content)
      pure m
    ) action

-- | Interpret the 'LLM' effect with a single repeating response.
runLLMConstant :: LLMResponse -> Eff (LLM : es) a -> Eff es a
runLLMConstant constantResp = interpret $ \env -> \case
  ChatComplete _ _ _ -> pure constantResp
  StreamTokens _ _ callback -> do
    localSeqUnlift env $ \unlift -> unlift (callback constantResp.message.content)
    pure constantResp
