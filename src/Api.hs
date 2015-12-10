{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           CRDT.TreeVector.Internal
import           Control.DeepSeq
import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import           Data.List
import           Data.Map as Map
import           Data.Monoid
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text
import           Data.Traversable
import           GHC.Generics
import           Safe
import qualified Servant.API as Method (Patch)
import           Servant.API hiding (Patch)

type PatchesApi =
  "api" :> (
    "fromServer" :> Get '[JSON] (TreeVector Char) :<|>
    "fromClient" :> ReqBody '[JSON] (TreeVector Char) :> Post '[JSON] ()
    ) :<|>
  Raw

patchesApi :: Proxy PatchesApi
patchesApi = Proxy

instance ToJSON a => ToJSON (TreeVector a) where
  toJSON (TreeVector m) =
    Object $ Data.List.foldl'
      (\ m (k, v) -> HashMap.insert (toKey k) (toJSON v) m) mempty $
    Map.toList m
    where
      toKey :: Client -> Text
      toKey (Client n) = cs $ show n

instance FromJSON a => FromJSON (TreeVector a) where
  parseJSON = \ case
    Object m -> do
      pairs <- forM (HashMap.toList m) $ \ (k, v) -> do
        case readMay (cs k) of
          Nothing -> fail ("cannot be parsed as a number: " ++ cs k)
          Just id -> do
            node <- parseJSON v
            return (Client id, node)
      return $ TreeVector $ Map.fromList pairs

instance ToJSON a => ToJSON (Node a)
instance FromJSON a => FromJSON (Node a)
instance ToJSON a => ToJSON (Element a)
instance FromJSON a => FromJSON (Element a)
instance NFData a => NFData (TreeVector a)
instance NFData a => NFData (Node a)
instance NFData a => NFData (Element a)
instance NFData CRDT.TreeVector.Internal.Client
