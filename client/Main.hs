{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-name-shadowing #-}

module Main where

import           Data.Aeson
import           Data.Map (insert)
import           Data.Patch
import           Data.String.Conversions
import qualified Data.Vector as V
import           Reflex.Dom

import           Api ()

main :: IO ()
main = do
  mainWidget $ el "div" $ do
    value <- _textArea_input <$> textArea def
    el "p" $ do
      debug value
      both <- diffOldNew "" value
      debug both
      let patches = fmap (uncurry mkDiff) both
      debug patches
      let requests = fmap mkRequest patches
      debug requests
      answers <- performRequestAsync requests
      debug answers

debug :: (Show a, MonadWidget t m) => Event t a -> m ()
debug a = do
  dyn <- holdDyn "" (fmap show a)
  el "p" $ dynText dyn

pText :: MonadWidget t m => Dynamic t String -> m ()
pText dyn = el "p" $ dynText dyn

diffOldNew :: MonadWidget t m => a -> Event t a -> m (Event t (a, a))
diffOldNew initial input = do
  dyn <- foldDyn acc [initial] input
  return $ fmapMaybe tuple $ updated dyn
  where
    acc :: a -> [a] -> [a]
    acc new l = case l of
      [] -> [new]
      [old] -> [old, new]
      (_ : old : _) -> [old, new]

    tuple :: [a] -> Maybe (a, a)
    tuple = \ case
      [old, new] -> Just (old, new)
      _ -> Nothing

mkDiff :: String -> String -> Patch Char
mkDiff a b = diff (V.fromList a) (V.fromList b)

mkRequest :: Patch Char -> XhrRequest
mkRequest patch = xhrRequest "POST" "/api/patch" def {
  _xhrRequestConfig_sendData = Just (cs (encode (toList patch))),
  _xhrRequestConfig_headers = insert "Content-Type" "application/json;charset=UTF-8"
    (_xhrRequestConfig_headers def)
}
