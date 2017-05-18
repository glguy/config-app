{-# Language OverloadedStrings #-}
module Main (main) where

import ConfigApp
import Language.Javascript.JSaddle.WKWebView (runFile)

main :: IO ()
main = runFile "index.html" "." jsMain
