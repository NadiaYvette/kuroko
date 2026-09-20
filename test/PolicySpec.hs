{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PolicySpec (spec) where

import Data.Aeson (object, (.=))
import Effectful
import Test.Hspec

import Kuroko

spec :: Spec
spec = describe "Kuroko.Effect.Policy" $ do
  let sampleCall = ToolCall "call-1" "query_db" (object ["query" .= ("SELECT 1" :: String)])
      sampleCall2 = ToolCall "call-2" "query_db" (object ["query" .= ("SELECT 2" :: String)])
      sampleCall3 = ToolCall "call-3" "query_db" (object ["query" .= ("SELECT 3" :: String)])

  describe "runToolPolicyAllowAll" $ do
    it "authorizes all tool calls unconditionally" $ do
      res <- runEff $ runToolPolicyAllowAll $ do
        v1 <- authorizeTool sampleCall
        v2 <- authorizeTool sampleCall2
        pure (v1, v2)
      res `shouldBe` (AllowAuto, AllowAuto)

  describe "runToolPolicyBudgetAndRate - Budget Caps" $ do
    it "denies execution when cumulative token budget is exceeded" $ do
      let budget = BudgetCap
            { maxTotalTokens = Just 100
            , maxToolCalls   = Nothing
            , maxCostCents   = Nothing
            }
      res <- runEff $ runToolPolicyBudgetAndRate budget Nothing $ do
        recordUsage (Usage 40 40 80)
        v1 <- authorizeTool sampleCall
        recordUsage (Usage 15 15 30) -- total now 110 >= 100
        v2 <- authorizeTool sampleCall2
        st <- getPolicyState
        pure (v1, v2, st.accumulatedTokens)
      case res of
        (AllowAuto, Denied msg, tokCount) -> do
          tokCount `shouldBe` 110
          msg `shouldBe` "Session total token budget exceeded."
        other -> expectationFailure $ "Expected (AllowAuto, Denied ..., 110) but got: " <> show other

    it "denies execution when cumulative tool call count is exceeded" $ do
      let budget = BudgetCap
            { maxTotalTokens = Nothing
            , maxToolCalls   = Just 2
            , maxCostCents   = Nothing
            }
      res <- runEff $ runToolPolicyBudgetAndRate budget Nothing $ do
        v1 <- authorizeTool sampleCall
        v2 <- authorizeTool sampleCall2
        v3 <- authorizeTool sampleCall3
        pure (v1, v2, v3)
      case res of
        (AllowAuto, AllowAuto, Denied msg) ->
          msg `shouldBe` "Session tool call budget exceeded."
        other -> expectationFailure $ "Expected (AllowAuto, AllowAuto, Denied ...) but got: " <> show other

  describe "runToolPolicyBudgetAndRate - Rate Caps" $ do
    it "throttles when calls exceed sliding window threshold" $ do
      let rate = RateCap
            { maxCallsPerWindow = 2
            , windowSeconds     = 10.0
            }
          budget = unlimitedBudget
      res <- runEff $ runToolPolicyBudgetAndRate budget (Just rate) $ do
        v1 <- authorizeTool sampleCall
        v2 <- authorizeTool sampleCall2
        v3 <- authorizeTool sampleCall3
        pure (v1, v2, v3)
      case res of
        (AllowAuto, AllowAuto, Throttled secs) ->
          secs `shouldSatisfy` (> 0.0)
        other -> expectationFailure $ "Expected (AllowAuto, AllowAuto, Throttled ...) but got: " <> show other

  describe "runToolPolicyMock" $ do
    it "evaluates arbitrary fine-grained tool policies" $ do
      let customPolicy call
            | call.toolName == "danger_zone" = Denied "Access forbidden by security policy"
            | call.toolName == "sudo"        = RequireConfirmation "Authorize root privilege?"
            | otherwise                      = AllowAuto
          tcDanger = ToolCall "c-danger" "danger_zone" (object [])
          tcSudo   = ToolCall "c-sudo" "sudo" (object [])
          tcRead   = ToolCall "c-read" "read_file" (object [])

      (v1, v2, v3) <- runEff $ runToolPolicyMock customPolicy $ do
        a <- authorizeTool tcDanger
        b <- authorizeTool tcSudo
        c <- authorizeTool tcRead
        pure (a, b, c)

      v1 `shouldBe` Denied "Access forbidden by security policy"
      v2 `shouldBe` RequireConfirmation "Authorize root privilege?"
      v3 `shouldBe` AllowAuto

  describe "ReAct Loop Integration with Policy Enforcement" $ do
    it "intercepts denied tool calls and informs LLM without executing tool" $ do
      let tcDenied = ToolCall "call-denied" "restricted_tool" (object [])
          step1Resp = LLMResponse
            { message = assistantMsg "" (Just [tcDenied])
            , usage   = Usage 10 10 20
            }
          step2Resp = LLMResponse
            { message = assistantMsg "I understand the tool is restricted." Nothing
            , usage   = Usage 10 10 20
            }
          policy call
            | call.toolName == "restricted_tool" = Denied "Permission denied"
            | otherwise = AllowAuto

          restrictedDef = ToolDef "restricted_tool" "Restricted" (object [])
          -- Handler should NOT be called if denied
          restrictedHandler _ = pure $ Right "Should never be executed!"
          reg = registerTool restrictedDef restrictedHandler emptyRegistry
          hist = [userMsg "Run restricted tool"]

      result <- runEff $
        runLLMMock [step1Resp, step2Resp] $
          runToolExecRegistry reg $
            runToolPolicyMock policy $
              runReActLoop 5 (ModelId "mock-model") hist

      length result `shouldBe` 4
      (result !! 2).role `shouldBe` RoleTool
      (result !! 2).content `shouldBe` "Policy Denied: Permission denied"
      (result !! 3).content `shouldBe` "I understand the tool is restricted."
