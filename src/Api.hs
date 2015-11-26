{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Aeson
import           Data.Patch
import           Data.Proxy
import           GHC.Generics
import           Servant.API

type PatchesApi =
       "api" :> "patch" :> ReqBody '[JSON] (Int, [Edit Char]) :> Post '[JSON] Message
  :<|> Raw

patchesApi :: Proxy PatchesApi
patchesApi = Proxy

deriving instance Generic (Edit a)
instance FromJSON a => FromJSON (Edit a)
instance ToJSON a => ToJSON (Edit a)

data Message
  = Success String
  | Error String
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
