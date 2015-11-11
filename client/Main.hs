
module Main where

import           Reflex.Dom

main :: IO ()
main = do
  mainWidget $ el "div" $ do
    text <- _textArea_value <$> textArea def
    el "p" $ do
      dynText text
