{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module MCPSpec (spec) where

import qualified Data.Aeson as Aeson
import Data.Aeson (object)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Effectful
import Effectful.MCP.Server (EffTool (..))
import MCP.Protocol
import MCP.Types
import Test.Hspec

import Kuroko

spec :: Spec
spec = describe "Kuroko.Effect.MCP" $ do
  it "converts Kuroko ToolDefs to typed MCP EffTools and executes them" $ do
    let schema = objectSchema
          [ ("expression", stringProp, "Math expression to evaluate", True)
          , ("precision", intProp, "Decimal precision", False)
          ]
        calcDef = ToolDef "calculator" "Evaluate mathematical expressions" schema
        calcHandler _ = pure $ Right "42"
        reg = registerTool calcDef calcHandler emptyRegistry

    res <- runEff $ runToolExecRegistry reg $ do
      defs <- listTools
      let effTools :: [EffTool '[ToolExec, IOE]] = kurokoToolsToEffTools defs
      case effTools of
        [] -> pure Nothing
        (EffTool tDef exec : _) -> do
          let args :: Map.Map Text Aeson.Value = Map.fromList [("expression", Aeson.String "6 * 7")]
          callRes <- exec (Just args)
          pure (Just (tDef.name, tDef.description, callRes))

    case res of
      Nothing -> expectationFailure "Expected at least one converted EffTool"
      Just (tName :: Text, tDesc, callRes :: CallToolResult) -> do
        tName `shouldBe` "calculator"
        tDesc `shouldBe` Just ("Evaluate mathematical expressions" :: Text)
        callRes.isError `shouldBe` Just False
        case callRes.content of
          [TextBlock tc] -> tc.text `shouldBe` "42"
          _              -> expectationFailure "Expected text content in tool result"

  it "handles tool failures properly as MCP errors" $ do
    let failDef = ToolDef "failTool" "Always fails" (object [])
        failHandler _ = pure $ Left "Execution failed: out of bounds"
        reg = registerTool failDef failHandler emptyRegistry

    res <- runEff $ runToolExecRegistry reg $ do
      defs <- listTools
      let effTools :: [EffTool '[ToolExec, IOE]] = kurokoToolsToEffTools defs
      case effTools of
        [EffTool _ exec] -> do
          callRes <- exec Nothing
          pure (Just callRes)
        _ -> pure Nothing

    case res of
      Nothing -> expectationFailure "Expected single converted EffTool"
      Just (callRes :: CallToolResult) -> do
        callRes.isError `shouldBe` Just True
        case callRes.content of
          [TextBlock tc] -> tc.text `shouldBe` "Execution failed: out of bounds"
          _              -> expectationFailure "Expected text error message"
