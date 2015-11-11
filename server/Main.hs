{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Shake.Ghcjs
import qualified System.Logging.Facade as Log

main :: IO ()
main = do
  app <- serveGhcjs $ BuildConfig {
    mainFile = "Main.hs",
    sourceDirs = [],
    projectDir = "../client",
    projectExec = Stack
  }
  let port = 8080
      settings =
        setPort port $
        setBeforeMainLoop (Log.info ("listening on port " ++ show port))
        defaultSettings
  runSettings settings app
