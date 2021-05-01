{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Week02.Homework1 where

import Control.Monad hiding (fmap)
import Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import qualified Ledger.Scripts as Scripts
import qualified Ledger.Typed.Scripts as Scripts
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract hiding (when)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Prelude (Semigroup (..))

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> (Bool, Bool) -> ValidatorCtx -> Bool
mkValidator _ (f, s) _ = f == s

data Typed

instance Scripts.ScriptType Typed where
  type DatumType Typed = ()
  type RedeemerType Typed = (Bool, Bool)

inst :: Scripts.ScriptInstance Typed
inst =
  Scripts.validator @Typed
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @() @(Bool, Bool)

validator :: Validator
validator = Scripts.validatorScript inst

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash

type GiftSchema =
  BlockchainActions
    .\/ Endpoint "give" Integer
    .\/ Endpoint "grab" (Bool, Bool)

give :: (HasBlockchainActions s, AsContractError e) => Integer -> Contract w s e ()
give amount = do
  let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
  ledgerTx <- submitTxConstraints inst tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => (Bool, Bool) -> Contract w s e ()
grab bs = do
  utxos <- utxoAt scrAddress
  let orefs = fst <$> Map.toList utxos
      lookups =
        Constraints.unspentOutputs utxos
          <> Constraints.otherScript validator
      tx :: TxConstraints Void Void
      tx = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData bs | oref <- orefs]
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
