{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- OPTIONS_GHC -Wall -Werror -fno-warn-name-shadowing #-}

module Main where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Patch
import           Data.String
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           GHC.Generics
import           Network.HTTP.Client
import           React.Flux
import           Servant.API hiding (Patch)
import           Servant.Client

import           Api

main :: IO ()
main = do
  store <- initStore
  reactRender "mainDocument" (intView store) ()

-- * store

data Store
  = Store {
    manager :: Manager,
    text :: Vector Char,
    lastPatch :: Maybe (Patch Char)
  }

initStore :: IO (ReactStore Store)
initStore = do
  manager <- newManager defaultManagerSettings
  return $ mkStore $ Store manager (V.fromList "") Nothing

-- * actions

data Action
  = SetText String
  deriving (Generic)

instance NFData Action

instance StoreData Store where
  type StoreAction Store = Action
  transform action (Store manager old _) = case action of
    SetText (V.fromList -> new) -> do
      let sendPatch :<|> _ = client patchesApi (BaseUrl Http "52.32.214.75" 8080 "") manager
      let patch = diff old new
      forkIO $ do
        reply <- runExceptT $ sendPatch (42, toList patch)
        print reply
      return $ Store manager new (Just patch)

-- * view

intView :: ReactStore Store -> ReactView ()
intView store = defineControllerView "text" store $ \ (Store _ text mPatch) () -> do
  textarea_ (
    "value" @= (V.toList text :: String) :
    onChange (\ event -> [SomeStoreAction store (SetText (target event "value"))]) :
    []) mempty
  p_ $ text_ $ fromString $ show text
  p_ $ pre_ $ fromString $ show mPatch
