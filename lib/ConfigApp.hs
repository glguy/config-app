{-# Language OverloadedStrings, ApplicativeDo #-}

module ConfigApp (
    jsMain
) where


import GHCJS.DOM (syncPoint, currentDocumentUnchecked)
import GHCJS.DOM.Types
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.EventM (on)
import qualified GHCJS.DOM.GlobalEventHandlers as E
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea

import qualified Data.Text as T
import Data.Semigroup
import qualified Data.List.NonEmpty as NE
import Data.Foldable
import Text.Show.Pretty (ppShow)

import Config
import Config.Schema
import Config.Schema.Load.Error


demoSpec :: ValueSpec (Maybe [Rational], Maybe Bool, Maybe T.Text, [(Int,Int)])
demoSpec = sectionsSpec "top-level configuration" $
  do nums <- optSection' "numbers" (oneOrList anySpec)
             "Try out the number syntax"
     bool <- optSection' "yes-or-no" yesOrNoSpec
             "Using atoms as enumerations"
     str  <- optSection "string"
             "Strings use Haskell syntax"
     xs   <- reqSection' "coordinates" (oneOrList nestedMapSpec)
             "Example required section of a nested map"
     return (nums,bool,str,xs)

nestedMapSpec :: ValueSpec (Int, Int)
nestedMapSpec = sectionsSpec "coord" $
  do x <- reqSection "x" "x coordinate"
     y <- reqSection "y" "y coordinate"
     return (x,y)

addDocumentation :: MonadJSM m => Document -> m ()
addDocumentation doc =
  do pre <- getThing doc HTMLPreElement "documentation"
     setInnerText pre (show (generateDocs demoSpec))

getThing :: (IsGObject a, MonadDOM m) => Document -> (JSVal -> a) -> String -> m a
getThing doc con eltId =
  do ~(Just e) <- getElementById doc eltId
     return $! uncheckedCastTo con e

jsMain :: JSM ()
jsMain = do
    doc <- currentDocumentUnchecked

    addDocumentation doc

    textarea <- getThing doc HTMLTextAreaElement "configinput"
    button   <- getThing doc HTMLButtonElement   "parse"
    output   <- getThing doc HTMLPreElement      "output"

    let selectError pos =
          do let i = posIndex pos
             TextArea.setSelectionRange textarea (Just i) (Just (i+1)) (Just ("forward" :: String))
             focus textarea

    _ <- on button E.click $ do
        txt <- TextArea.getValue textarea
        case parse txt of

          Left (ParseError pos e) ->
            do selectError pos
               setInnerText output $ unlines
                 [ "Parser error"
                 , showPosition pos ++ e ]

          Right x ->
            case loadValue demoSpec x of
              Left es ->
                do selectError (let ValueSpecMismatch p _ _ = es in p)
                   setInnerText output $ unlines
                     [ "Schema error"
                     , show (prettyValueSpecMismatch es) ]

              Right y -> setInnerText output $ unlines
                           [ "Success" , ppShow y ]

    syncPoint

showPosition :: Position -> String
showPosition pos = show (posLine pos) ++ ":" ++ show (posColumn pos) ++ ": "
