module Main
  ( main
  ) where

import Prelude

import Deku.DOM.Self as Self

import ToDeku (toDeku)
import Data.Array (intercalate, uncons)
import Data.Array as A
import Data.Compactable (compact)
import Data.Either (Either(..), either)
import Data.List (List(..))
import Deku.Hooks (useState')
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split, drop, take)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Toplevel (runInBody)
import Data.Tuple.Nested ((/\))
import Deku.DOM.Attributes as DA
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.DOM.Listeners as DL
import Deku.Toplevel (runInBody')
import Effect (Effect)
import Effect.Random (random)
import Deku.Toplevel (runInBody)
import Dodo (plainText, print, twoSpaces)
import Effect (Effect)
import HalogenParser (HtmlAttribute(..), HtmlNode(..))
import HalogenParser as HalogenParser
import Partial.Unsafe (unsafePartial)
import Swal (swal)
import Tidy (FormatOptions, defaultFormatOptions, formatExpr, toDoc)
import Tidy.Codegen (binaryOp, exprApp, exprArray, exprCtor, exprIdent, exprOp, exprString)
import Web.HTML.HTMLTextAreaElement (value)
import Yarn (capitalize)
import Deku.DOM.Listeners as DL
import Effect.Exception

initialTxt :: String
initialTxt =
  """<div>
  <button id="my-button" class="p-4">press me</button>
  <span class="text-slate:700">hello world!</span>
</div>"""

main :: Effect Unit
main = do
  initialParsed <- either (\error -> throw $ show error) pure $ map toDeku $
    HalogenParser.parse initialTxt
  runInBody Deku.do
    setPurs /\ purs <- useState initialParsed
    setInput /\ input <- useState'
    D.div
      [ DA.klass_ "w-full" ]
      [ D.span [ DA.klass_ "text-xl" ] [ text_ "html2deku" ]
      , D.button
          [ DA.klass_
              "ml-2 inline-flex items-center rounded border border-transparent bg-indigo-600 px-2.5 py-1.5 text-xs font-medium text-white shadow-sm hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2"
          , DL.runOn DL.click $ input <#> \i -> do
              v <- value i
              let parsed = HalogenParser.parse v
              case parsed of
                Left err -> swal { title: "Uh oh...", text: show err }
                Right res -> setPurs (toDeku res)
          ]
          [ text_ "Convert >" ]
      , D.div
          [ DA.klass_ "w-full grid grid-cols-1 md:grid-cols-2 gap-4" ]
          [ D.textarea
              [ DA.rows_ "20"
              , DA.klass_ "border-2"
              , Self.selfT_ setInput
              ]
              [ text_ initialTxt ]
          , D.textarea
              [ DA.rows_ "20"
              , DA.klass_ "border-2"
              ]
              [ text purs ]
          ]
      ]
