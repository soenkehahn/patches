{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- OPTIONS_GHC -Wall -Werror -fno-warn-name-shadowing #-}

module Main where

import           CRDT.TreeVector as CRDT
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception hiding (try)
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.List
import           Data.Monoid
import           Data.String
import           GHC.Generics
import           GHCJS.Prim (fromJSString, fromJSInt)
import           JavaScript.Object
import           React.Flux
import           Safe
import           Servant.API hiding (Patch)
import           Servant.Client
import           System.IO.Unsafe
import           System.Random

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
  let port = case js_port of
        "" -> case protocol of
          Http -> 80
          Https -> 443
        _ -> case readMay js_port of
          Just p -> p
          Nothing -> error ("unparseable port: " ++ js_port)
  return $ BaseUrl protocol host port path

foreign import javascript unsafe "(function () { return location; })()"
  js_location :: IO Object

{-# NOINLINE fromServer #-}
{-# NOINLINE fromClient #-}
(fromServer :<|> fromClient) :<|> _ = unsafePerformIO $ do
  baseUrl <- sameOriginBaseUrl ""
  return $ client patchesApi baseUrl (error "manager")

-- * main

main :: IO ()
main = do
  c <- Client <$> randomRIO (0, 2 ^ 15)
  alterStore store (SetClient c)
  reactRender "mainDocument" mainView ()
  forever $ do
    update <- try $ fromServer
    alterStore store $ FromServer update
    threadDelay 1000000

-- * store

data Store
  = Store {
    c :: CRDT.Client,
    text :: String,
    tree :: TreeVector Char
  }
  deriving (Show)

store :: ReactStore Store
store = mkStore $ Store (error "uninitialized client") "" mempty

try :: ExceptT ServantError IO a -> IO a
try action = do
  r <- runExceptT action
  case r of
    Left err -> throwIO (ErrorCall (show err))
    Right a -> return a

-- * actions

data Action
  = SetClient CRDT.Client -- fixme: better client setting?
  | FromServer (TreeVector Char)
  | FromEditor String
  deriving (Generic)

instance NFData Action

instance StoreData Store where
  type StoreAction Store = Action
  transform action (Store client text tree) = case action of
    SetClient c -> return $ Store c text tree
    FromEditor t -> do
      let new = tree <> mkPatch client tree t
      forkIO $ try $ fromClient new
      return $ Store client t new
    FromServer update -> do
      let newTree = tree <> update
      return $ Store client (getVector newTree) newTree

-- * view

mainView :: ReactView ()
mainView = defineControllerView "mainView" store $ \ store () -> do
  view editorView store mempty

editorView :: ReactView Store
editorView = defineView "editorView" $ \ (Store client text tree) ->
  textarea_ (
    "value" @= text :
    onChange (\ event ->
      let text = target event "value"
      in ([SomeStoreAction store (FromEditor text)])) :
    []) mempty
