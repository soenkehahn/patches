{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
    production :: Bool,
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
      env = if production options
        then Production
        else Development
  runSettings settings =<< mkApp env

mkApp :: Environment -> IO Application
mkApp env = do
  jsIndexApp <- $(serveGhcjs $ BuildConfig {
    mainFile = "Main.hs",
    sourceDirs = [".", "../src"],
    projectDir = "client",
    projectExec = Stack,
    buildDir = "js-builds"
  }) env
  mvar <- newMVar mempty
  return $ \ request respond -> do
    (serve patchesApi (api mvar :<|> jsIndexApp)) request respond

api :: MVar (Vector Char) -> [Edit Char] -> ExceptT ServantErr IO Message
api mvar edits = liftIO $ modifyMVar mvar $ \ document -> do
  let patch = unsafeFromList edits
      newDocument = apply patch document
  return (newDocument, Success $ Data.Vector.toList newDocument)
