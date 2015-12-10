{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

import           CRDT.TreeVector
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Maybe
import           Data.Monoid
import           Network.Wai
import           Network.Wai.Ghcjs
import           Network.Wai.Handler.Warp
import           Servant hiding (Patch)
import qualified System.Logging.Facade as Log
import           WithCli

import           Api

data Options
  = Options {
    port :: Int
  }
  deriving (Show, Generic, HasArguments)

main :: IO ()
main = withCli $ \ options -> do
  let settings =
        setPort (port options) $
        setBeforeMainLoop
          (Log.info ("listening on port " ++ show (port options)))
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = do
  jsIndexApp <- $(serveGhcjs (BuildConfig {
    mainFile = "Main.hs",
    customIndexFile = Just "index.html",
    sourceDirs = [".", "../src"],
    projectDir = "client",
    projectExec = Stack,
    buildDir = "js-builds"
  }))
  mvar <- newMVar initialState
  let server :: Server PatchesApi
      server =
        (fromServer mvar :<|>
         fromClient mvar) :<|>
        jsIndexApp
  return $ serve patchesApi server

type DB = MVar State

data State
  = State {
    tree :: TreeVector Char
  }

initialState :: State
initialState = State mempty

fromServer :: MVar State -> ExceptT ServantErr IO (TreeVector Char)
fromServer mvar = liftIO $ tree <$> readMVar mvar

fromClient :: MVar State -> TreeVector Char -> ExceptT ServantErr IO ()
fromClient mvar clientTree = liftIO $ modifyMVar_ mvar $ \ (State tree) -> do
  let new = tree <> clientTree
  return $ State new
