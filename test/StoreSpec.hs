{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreSpec (spec) where

import Data.Aeson (object, (.=))
import Effectful
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Kuroko

spec :: Spec
spec = describe "Kuroko.Effect.Store" $ do
  it "persists session history and retrieves messages in chronological order" $ do
    withSystemTempDirectory "kuroko-test-db" $ \tmpDir -> do
      let dbPath = tmpDir </> "kuroko.sqlite"
      (history, _) <- runEff $ runSessionStoreSqlite dbPath $ do
        sid <- createSession "Test Agent Run"
        addMessage sid (userMsg "Hello, agent!")
        let tc = ToolCall "call-101" "read_file" (object ["path" .= ("/tmp/test.txt" :: String)])
        addMessage sid (assistantMsg "Reading file..." (Just [tc]))
        addMessage sid (toolResultMsg "call-101" "file contents here")
        addMessage sid (assistantMsg "Summary of file contents" Nothing)
        auditTool sid "read_file" (object ["path" .= ("/tmp/test.txt" :: String)]) "file contents here" True
        hist <- getHistory sid
        pure (hist, sid)

      length history `shouldBe` 4
      (history !! 0).role `shouldBe` RoleUser
      (history !! 0).content `shouldBe` "Hello, agent!"

      (history !! 1).role `shouldBe` RoleAssistant
      (history !! 1).toolCalls `shouldBe` Just [ToolCall "call-101" "read_file" (object ["path" .= ("/tmp/test.txt" :: String)])]

      (history !! 2).role `shouldBe` RoleTool
      (history !! 2).content `shouldBe` "file contents here"
      (history !! 2).toolCallId `shouldBe` Just "call-101"

      (history !! 3).role `shouldBe` RoleAssistant
      (history !! 3).content `shouldBe` "Summary of file contents"
