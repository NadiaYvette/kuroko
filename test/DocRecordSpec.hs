{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module DocRecordSpec (spec) where

import Data.DocRecord
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

type TaskNameField = '["task"] :|: Text
type PriorityField = '["priority"] :|: Int
type SafeModeField = '["safe_mode"] :|: Bool

type AgentTaskContext = DocRec '[TaskNameField, PriorityField, SafeModeField]

renderContextPrompt :: AgentTaskContext -> Text
renderContextPrompt r =
  "Task: " <> (r ^^?! Proxy @TaskNameField) <>
  " (Priority: " <> Text.pack (show (r ^^?! Proxy @PriorityField)) <>
  ", SafeMode: " <> Text.pack (show (r ^^?! Proxy @SafeModeField)) <> ")"

spec :: Spec
spec = describe "Data.DocRecord (Extensible Typed Agent Context)" $ do
  it "constructs a typed documented record and retrieves fields via lens" $ do
    let taskFld = docField @"task" ("Refactor codebase" :: Text) "Name of the agent task"
        priorityFld = docField @"priority" (9 :: Int) "Priority level from 1-10"
        safeModeFld = docField @"safe_mode" True "Whether destructive tools are blocked"
        ctx :: AgentTaskContext
        ctx = taskFld :& priorityFld :& safeModeFld :& RNil

    (ctx ^^?! Proxy @TaskNameField) `shouldBe` "Refactor codebase"
    (ctx ^^?! Proxy @PriorityField) `shouldBe` 9
    (ctx ^^?! Proxy @SafeModeField) `shouldBe` True

  it "renders prompt template from extensible typed record" $ do
    let ctx :: AgentTaskContext
        ctx = docField @"task" ("Security Audit" :: Text) "Name of the task"
           :& docField @"priority" (10 :: Int) "Priority"
           :& docField @"safe_mode" False "Safe Mode"
           :& RNil

    renderContextPrompt ctx `shouldBe` "Task: Security Audit (Priority: 10, SafeMode: False)"
