{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Example

import Config
import Data.JSString
import Data.JSString.Text
import GHCJS.Foreign.Callback
import GHCJS.Types

foreign import javascript unsafe "$1.addEventListener('click', $2)"
  addClickListener :: JSVal -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$r = document.getElementById($1);"
  getElement :: JSString -> IO JSVal

foreign import javascript unsafe "$r = $1.value;"
  getValue :: JSVal -> IO JSString

foreign import javascript unsafe "console.log($1);"
  clog :: JSString -> IO ()

main :: IO ()
main = do
  area <- getElement "area"
  getElement "btn" >>= \btn ->
    (addClickListener btn =<< asyncCallback (go area))
    where
      go area = do
        areaValue <- textFromJSString <$> getValue area
        clog $ pack $ show (parse areaValue)

        
    
