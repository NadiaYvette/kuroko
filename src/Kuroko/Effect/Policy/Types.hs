{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kuroko.Effect.Policy.Types
  ( PolicyVerdict (..)
  , BudgetCap (..)
  , unlimitedBudget
  , RateCap (..)
  , PolicyConfig (..)
  , defaultPolicyConfig
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Dhall
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- | Authorization verdict for a tool invocation.
data PolicyVerdict
  = AllowAuto
  | Throttled !Double              -- ^ Rate limit reached: throttled wait duration in seconds
  | Denied !Text                   -- ^ Budget or permission denial reason
  | RequireConfirmation !Text     -- ^ Operator manual approval needed
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Budget cap constraints for autonomous sessions.
data BudgetCap = BudgetCap
  { maxTotalTokens :: !(Maybe Natural)   -- ^ Cumulative total token ceiling across turns
  , maxToolCalls   :: !(Maybe Natural)   -- ^ Cumulative tool invocation count ceiling
  , maxCostCents   :: !(Maybe Double)    -- ^ Financial expenditure ceiling in cents USD
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, Dhall.FromDhall, Dhall.ToDhall)

-- | Smart constructor for unconstrained budget.
unlimitedBudget :: BudgetCap
unlimitedBudget = BudgetCap
  { maxTotalTokens = Nothing
  , maxToolCalls   = Nothing
  , maxCostCents   = Nothing
  }

-- | Sliding-window rate cap configuration.
data RateCap = RateCap
  { maxCallsPerWindow :: !Natural -- ^ Maximum calls permitted in the rolling window
  , windowSeconds     :: !Double  -- ^ Window duration in seconds
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, Dhall.FromDhall, Dhall.ToDhall)

-- | Comprehensive tool policy configuration.
data PolicyConfig = PolicyConfig
  { budgetCap :: !BudgetCap
  , rateCap   :: !(Maybe RateCap)
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, Dhall.FromDhall, Dhall.ToDhall)

-- | Default recommended policy configuration.
defaultPolicyConfig :: PolicyConfig
defaultPolicyConfig = PolicyConfig
  { budgetCap = BudgetCap
      { maxTotalTokens = Just 50000
      , maxToolCalls   = Just 25
      , maxCostCents   = Just 100.0
      }
  , rateCap = Just (RateCap 10 60.0)
  }
