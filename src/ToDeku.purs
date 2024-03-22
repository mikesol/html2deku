module ToDeku where

import Prelude

import Data.Array (intercalate, uncons)
import Data.Array as A
import Data.Compactable (compact)
import Data.Either (Either(..))
import Data.List (List(..))
import Deku.Hooks (useState')
import Data.Maybe (Maybe(..))
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

dekuizeUppercase :: String -> String
dekuizeUppercase = dekuize true

dekuizeLowercase :: String -> String
dekuizeLowercase = dekuize false

dekuize :: Boolean -> String -> String
dekuize ul i = o
  where
  splt = split (Pattern "-") i
  uc = uncons splt
  o = case uc of
    Nothing -> ""
    Just { head, tail } -> intercalate ""
      ([ (if ul then capitalize else identity) head ] <> map capitalize tail)

toDeku :: List HtmlNode -> String
toDeku l = print plainText
  twoSpaces
  ( toDoc
      ( formatExpr (defaultFormatOptions :: FormatOptions Void _)
          case l of
            Nil -> exprArray []
            Cons a Nil -> case go a of
              Just x -> x
              Nothing -> exprArray []
            _ -> unsafePartial $ exprApp (exprIdent "fixed")
              [ exprArray (compact (map go (A.fromFoldable l))) ]
      )
  )
  where
  go (HtmlElement { name, attributes, children }) = Just $ unsafePartial $
    exprApp
      ( exprIdent
          ( "D." <> dekuizeLowercase name <> case attributes of
              Nil -> "_"
              _ -> ""
          )
      )
      ( ( let
            transAp (HtmlAttribute k' v) =
              if take 5 k' == "data-" then exprApp (exprIdent "pure")
                [ exprApp (exprIdent "xdata")
                    [ exprString $ drop 5 k', exprString v ]
                ]
              else
                let
                  k =
                    case k' of
                      "type" -> "xtype"
                      "class" -> "klass"
                      _ -> k'
                in
                  exprApp (exprIdent ("DA." <> dekuizeLowercase k <> "_"))
                    [ exprString v ]
          in
            case attributes of
              Nil -> []
              _ ->
                [ exprArray $ A.fromFoldable
                    ( map
                        transAp
                        attributes
                    )
                ]
        ) <>
          [ exprArray (compact (map go (A.fromFoldable children))) ]
      )
  go (HtmlText str) =
    let
      nw = replaceAll (Pattern "\n") (Replacement "")
        (replaceAll (Pattern " ") (Replacement "") str)
    in
      if nw == "" then Nothing
      else Just
        (unsafePartial $ exprApp (exprIdent "text_") [ exprString str ])
  go (HtmlComment _) = Nothing
