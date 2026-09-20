{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module MockSpec (spec) where

import Data.Aeson (object, (.=))
import Effectful
import Test.Hspec

import Kuroko

spec :: Spec
spec = describe "Kuroko.Workflow.ReAct (Mock Suite)" $ do
  it "completes single-turn conversation without tools" $ do
    let mockResp = LLMResponse
          { message = assistantMsg "Hello from Kuroko!" Nothing
          , usage   = Usage 5 10 15
          }
        hist = [userMsg "Hi!"]
    result <- runEff $
      runLLMMock [mockResp] $
        runToolExecRegistry emptyRegistry $
          runToolPolicyAllowAll $
            runReActLoop 3 (ModelId "mock-model") hist
    length result `shouldBe` 2
    (last result).content `shouldBe` "Hello from Kuroko!"

  it "executes a tool call and feeds result back to LLM" $ do
    let toolCall = ToolCall "call-1" "echo" (object ["msg" .= ("hello world" :: String)])
        step1Resp = LLMResponse
          { message = assistantMsg "" (Just [toolCall])
          , usage   = Usage 10 10 20
          }
        step2Resp = LLMResponse
          { message = assistantMsg "Echo completed successfully." Nothing
          , usage   = Usage 20 10 30
          }
        echoDef = ToolDef "echo" "Echo input" (object [])
        echoHandler call = pure $ Right ("Echoed: " <> call.toolCallId)
        reg = registerTool echoDef echoHandler emptyRegistry
        hist = [userMsg "Please echo"]

    result <- runEff $
      runLLMMock [step1Resp, step2Resp] $
        runToolExecRegistry reg $
          runToolPolicyAllowAll $
            runReActLoop 5 (ModelId "mock-model") hist

    -- Expected history: User -> Assistant (with tool call) -> Tool Result -> Assistant final
    length result `shouldBe` 4
    (result !! 0).role `shouldBe` RoleUser
    (result !! 1).role `shouldBe` RoleAssistant
    (result !! 2).role `shouldBe` RoleTool
    (result !! 2).content `shouldBe` "Echoed: call-1"
    (result !! 3).role `shouldBe` RoleAssistant
    (result !! 3).content `shouldBe` "Echo completed successfully."
