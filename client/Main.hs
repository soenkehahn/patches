
module Main where

import           Data.Map (insert)
import           Reflex.Dom

main :: IO ()
main = do
  mainWidget $ el "div" $ do
    value <- _textArea_input <$> textArea def
    debug value
    answer <- performRequestAsync (fmap mkRequest value)
    debug answer

mkRequest :: String -> XhrRequest
mkRequest value = xhrRequest "POST" "/api" def {
  _xhrRequestConfig_sendData = Just value,
  _xhrRequestConfig_headers = insert "Content-Type" "application/json;charset=UTF-8"
    (_xhrRequestConfig_headers def)
}

debug :: (Show a, MonadWidget t m) => Event t a -> m ()
debug a = do
  dyn <- holdDyn "" (fmap show a)
  el "p" $ el "pre" $ dynText dyn
