{-# Language OverloadedStrings, ApplicativeDo #-}

module Main (
    main
) where


import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)

import GHCJS.DOM (syncPoint, currentDocumentUnchecked)
import GHCJS.DOM.Types
import GHCJS.DOM.Document (getHeadUnsafe, getBodyUnsafe, createElement, createTextNode)
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.EventM (on, mouseClientXY)
import GHCJS.DOM.GlobalEventHandlers as E
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea

import qualified Data.Text as T
import Data.Semigroup
import Data.Foldable
import Control.Exception

import Config
import Config.Schema

--import Language.Javascript.JSaddle.WKWebView (run)
run = id

main = run helloMain

myHtml :: String
myHtml = "<pre>" ++ show (generateDocs demoSpec) ++
         "</pre>\
         \<h2>Configuration file input</h2>"

startingInput :: T.Text
startingInput =
  "numbers: [1.0,2e5,-3]\n\
  \yes-or-no: yes\n\
  \string: \"a string literal\"\n\
  \coordinates:\n\
  \  * x: 10\n\
  \    y: 20\n\
  \  * x: 3\n\
  \    y: 5\n"

demoSpec :: ValueSpecs (Maybe [Rational], Maybe Bool, Maybe T.Text, [(Int,Int)])
demoSpec = sectionsSpec "top-level configuration" $
  do nums <- optSection' "numbers" (oneOrList valuesSpec)
             "Try out the number syntax"
     bool <- optSection' "yes-or-no" yesOrNoSpec
             "Using atoms as enumerations"
     str  <- optSection "string"
             "Strings use Haskell syntax"
     xs   <- reqSection' "coordinates" (oneOrList nestedMapSpec)
             "Example required section of a nested map"
     return (nums,bool,str,xs)

nestedMapSpec :: ValueSpecs (Int, Int)
nestedMapSpec = sectionsSpec "coord" $
  do x <- reqSection "x" "x coordinate"
     y <- reqSection "y" "y coordinate"
     return (x,y)

addDocumentation doc body =
  do pre <- uncheckedCastTo HTMLPreElement
           <$> createElement doc ("pre" :: String)
     setInnerText pre (Just (show (generateDocs demoSpec)))
     appendChild body pre

helloMain :: JSM ()
helloMain = do
    doc <- currentDocumentUnchecked
    body <- getBodyUnsafe doc

    addDocumentation doc body

    heading  <- uncheckedCastTo HTMLHeadingElement
             <$> createElement doc ("h2" :: String)
    textarea <- uncheckedCastTo HTMLTextAreaElement
             <$> createElement doc ("textarea" :: String)
    button   <- uncheckedCastTo HTMLButtonElement
             <$> createElement doc ("button" :: String)
    output   <- uncheckedCastTo HTMLPreElement
             <$> createElement doc ("pre" :: String)

    setId textarea ("inputbox" :: String)
    setAttribute textarea ("rows" :: String) ("10" :: String)
    setAttribute textarea ("cols" :: String) ("50" :: String)
    setAttribute textarea ("style" :: String) ("font-family: monospace; font-size: large;" :: String)
    TextArea.setValue textarea (Just startingInput)

    setInnerText heading (Just ("Configuration file input" :: String))

    appendChild body heading
    appendChild body textarea
    appendChild body button
    appendChild body output
    setInnerHTML button (Just ("Parse" :: String))

    _ <- on button E.click $ do
        Just txt <- TextArea.getValue textarea
        case parse (txt <> "\n") of

          Left (ParseError pos e) ->
            setInnerText output $ Just $ unlines
              [ "Parser error"
              , showPosition pos ++ e ]

          Right x ->
            case loadValue demoSpec x of
              Left es ->
                 setInnerText output $ Just $ unlines
                   $ "Schema error"
                   : map showLoadError (toList es)

              Right y -> setInnerText output $ Just $ unlines
                           [ "Success" , show y ]
        return ()

    syncPoint

showLoadError :: LoadError -> String
showLoadError (LoadError pos path problem) =
  showPosition pos ++
  T.unpack (T.concat (map (<>":") path)) ++
  " " ++
  showProblem problem

showPosition :: Position -> String
showPosition pos = show (posLine pos) ++ ":" ++ show (posColumn pos) ++ ": "

showProblem :: Problem -> String
showProblem (MissingSection x) = "missing required section `" ++ T.unpack x ++ "`"
showProblem (UnusedSection  x) = "unused section `"           ++ T.unpack x ++ "`"
showProblem (SpecMismatch   x) = "expected "                  ++ T.unpack x
