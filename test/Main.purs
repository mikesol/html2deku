module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import HalogenParser (HtmlAttribute(..), HtmlNode(..))
import HalogenParser as HalogenParser
import Prelude

import Data.Either
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import ToDeku (toDeku)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Effect.Exception (Error, error)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  it "awesome" do
    let
      input :: String
      input =
        """<div>
  <button id="my-button" class="p-4">press me</button>
  <span class="text-slate:700">hello world!</span>
</div>"""

      output :: String
      output =
        """D.div_
  [ D.button [ DA.id_ "my-button", DA.klass_ "p-4" ] [ text_ "press me" ]
  , D.span [ DA.klass_ "text-slate:700" ] [ text_ "hello world!" ]
  ]"""

    input `shouldParseTo` output

shouldParseTo :: forall m. MonadThrow Error m => String -> String -> m Unit
shouldParseTo input output =
  let
    parsed = HalogenParser.parse input
  in
    case parsed of
      Left err -> fail $ show err
      Right res -> output `shouldEqual` toDeku res
