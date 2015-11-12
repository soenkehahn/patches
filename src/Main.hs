{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Patch
import           Data.Vector (Vector, toList)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Shake.Ghcjs
import           Servant hiding (Patch)
import qualified System.Logging.Facade as Log
import           WithCli

import           Api

data Options
  = Options {
    production :: Bool
  }
  deriving (Show, Generic, HasArguments)

main :: IO ()
main = withCli $ \ options -> do
  print (options :: Options)
  let port = 8080
      settings =
        setPort port $
        setBeforeMainLoop (Log.info ("listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = do
  jsIndexApp <- serveGhcjs $ BuildConfig {
    mainFile = "Main.hs",
    sourceDirs = [".", "../src"],
    projectDir = "client",
    projectExec = Stack
  }
  mvar <- newMVar mempty
  return $ \ request respond -> do
    Log.info $ show request
    (serve patchesApi (api mvar :<|> jsIndexApp)) request respond

api :: MVar (Vector Char) -> [Edit Char] -> ExceptT ServantErr IO Message
api mvar edits = liftIO $ modifyMVar mvar $ \ document -> do
  let patch = unsafeFromList edits
      newDocument = apply patch document
  liftIO $ Log.info $ show patch
  liftIO $ Log.info $ show newDocument
  return (newDocument, Success $ Data.Vector.toList newDocument)
