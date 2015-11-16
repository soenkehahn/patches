{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Proxy
import           Servant.API

type PatchesApi =
       "api" :> Raw
  :<|> Raw

patchesApi :: Proxy PatchesApi
patchesApi = Proxy
