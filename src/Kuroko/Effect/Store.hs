{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Kuroko.Effect.Store
  ( SessionStore (..)
  , createSession
  , addMessage
  , getHistory
  , auditTool
  , Session (..)
  , StoredMessage (..)
  , ToolExecutionAudit (..)
  , migrateAll
  , runSessionStoreSqlite
  ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH

import Kuroko.Core.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Session
    title Text
    createdAt UTCTime
    deriving Show Eq
StoredMessage
    sessionId SessionId
    role Text
    content Text
    toolCallsJson Text Maybe
    toolCallId Text Maybe
    createdAt UTCTime
    deriving Show Eq
ToolExecutionAudit
    sessionId SessionId
    toolName Text
    argsJson Text
    resultText Text
    exitSuccess Bool
    executedAt UTCTime
    deriving Show Eq
|]

-- | Effect for managing durable session histories and tool audit journals.
data SessionStore :: Effect where
  CreateSession :: Text -> SessionStore m SessionId
  AddMessage    :: SessionId -> Message -> SessionStore m ()
  GetHistory    :: SessionId -> SessionStore m [Message]
  AuditTool     :: SessionId -> Text -> Aeson.Value -> Text -> Bool -> SessionStore m ()

makeEffect ''SessionStore

-- | Interpret 'SessionStore' using an SQLite database file.
runSessionStoreSqlite :: IOE :> es => FilePath -> Eff (SessionStore : es) a -> Eff es a
runSessionStoreSqlite dbPath action = do
  liftIO $ runSqlite (Text.pack dbPath) $ runMigration migrateAll
  interpret (\_ -> \case
    CreateSession title -> liftIO $ do
      now <- getCurrentTime
      runSqlite (Text.pack dbPath) $ insert (Session title now)
    AddMessage sid msg -> liftIO $ do
      now <- getCurrentTime
      let tcJson = case msg.toolCalls of
            Just tcs -> Just (TE.decodeUtf8 (BSL.toStrict (Aeson.encode tcs)))
            Nothing  -> Nothing
          roleStr = case msg.role of
            RoleUser      -> "user"
            RoleSystem    -> "system"
            RoleAssistant -> "assistant"
            RoleTool      -> "tool"
      runSqlite (Text.pack dbPath) $
        insert_ (StoredMessage sid roleStr msg.content tcJson msg.toolCallId now)
    GetHistory sid -> liftIO $ do
      stored <- runSqlite (Text.pack dbPath) $
        selectList [StoredMessageSessionId ==. sid] [Asc StoredMessageCreatedAt]
      pure $ map toMessage (map entityVal stored)
    AuditTool sid name args res ok -> liftIO $ do
      now <- getCurrentTime
      let argsStr = TE.decodeUtf8 (BSL.toStrict (Aeson.encode args))
      runSqlite (Text.pack dbPath) $
        insert_ (ToolExecutionAudit sid name argsStr res ok now)
    ) action
  where
    toMessage (StoredMessage _ r c mtc mcid _) =
      let parsedRole = case r of
            "user"      -> RoleUser
            "system"    -> RoleSystem
            "assistant" -> RoleAssistant
            "tool"      -> RoleTool
            _           -> RoleUser
          parsedTcs = case mtc of
            Just raw -> Aeson.decodeStrict (TE.encodeUtf8 raw)
            Nothing  -> Nothing
      in Message parsedRole c parsedTcs mcid
