{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LouterIntegrationSpec (spec) where

import Data.Aeson (object, (.=))
import Effectful
import Test.Hspec

import Kuroko
import Kuroko.Effect.LLM.Louter
import Louter.Client.Servant (LouterClientConfig(..))
import qualified Louter.Core.Types as LouterTypes

spec :: Spec
spec = describe "Kuroko.Effect.LLM.Louter Integration" $ do
  it "initializes default configurations for OpenAI, Anthropic, and Gemini" $ do
    let openAICfg = louterOpenAIConfig "test-key"
        anthropicCfg = louterAnthropicConfig "test-key"
        geminiCfg = louterGeminiConfig "test-key"
    openAICfg.defaultModel `shouldBe` "gpt-4o"
    anthropicCfg.defaultModel `shouldBe` "claude-3-5-sonnet-20241022"
    geminiCfg.defaultModel `shouldBe` "gemini-1.5-pro"
