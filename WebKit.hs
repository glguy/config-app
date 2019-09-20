{-# Language OverloadedStrings #-}
module Main (main) where

import ConfigApp
import Language.Javascript.JSaddle.WKWebView (runFile)
import Data.Default

main :: IO ()
main = runFile "index.html" "." def jsMain
