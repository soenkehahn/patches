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
import           Control.Exception hiding (try)
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Patch
import           Data.String
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           GHC.Generics
import           GHCJS.Prim (fromJSString, fromJSInt)
import           JavaScript.Object
import           Network.HTTP.Client
import           React.Flux
import           Safe
import           Servant.API hiding (Patch)
import           Servant.Client
import           System.IO.Unsafe

import           Api

-- * api calls

sameOriginBaseUrl :: String -> IO BaseUrl
sameOriginBaseUrl path = do
  location <- js_location
  js_protocol <- fromJSString <$> getProp "protocol" location
  let protocol = case js_protocol of
        "http:" -> Http
        "https:" -> Https
        _ -> error ("unparseable protocol: " ++ js_protocol)
  host <- fromJSString <$> getProp "hostname" location
  js_port <- fromJSString <$> getProp "port" location
  let port = case readMay js_port of
        Just p -> p
        Nothing -> error ("unparseable port: " ++ js_port)
  return $ BaseUrl protocol host port path

foreign import javascript unsafe "(function () { return location; })()"
  js_location :: IO Object

(getDocument :<|> sendPatch) :<|> _ = unsafePerformIO $ do
  baseUrl <- sameOriginBaseUrl ""
  return $ client patchesApi baseUrl (error "manager")

-- * main

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
  doc <- try $ getDocument
  return $ mkStore $ Store manager (V.fromList doc) Nothing

try :: ExceptT ServantError IO a -> IO a
try action = do
  r <- runExceptT action
  case r of
    Left err -> throwIO (ErrorCall (show err))
    Right a -> return a

-- * actions

data Action
  = SetText String
  deriving (Generic)

instance NFData Action

instance StoreData Store where
  type StoreAction Store = Action
  transform action (Store manager old _) = case action of
    SetText (V.fromList -> new) -> do
      let patch = diff old new
      forkIO $ do
        reply <- runExceptT $ sendPatch (toList patch)
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
