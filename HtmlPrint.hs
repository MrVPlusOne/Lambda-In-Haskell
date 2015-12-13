module HtmlPrint
    (
      inferConstraintHtml,
      mkPage
    ) where

import LambdaTerm
import TypedTerm

type Html = String

varHtml name t = "<span title=\"" ++ name ++ ": " ++ show t ++ "\">" ++ name ++ "</span>"

keyWordHtml w = "<span class='keyword'>" ++ w ++ "</span>"

typeHtml t= concat ["<span class='type'>", show t, "</span>"]

htmlTypeTree :: Term -> NamedTyTerm -> Html
htmlTypeTree term tree =
  case (term, tree) of
    (Var v, Var t) -> varHtml v t
    (Apply x y, Apply tx ty) -> surroundAbstr x tx ++ " " ++ surroundNonPrim y ty
    (Abstr v body, Abstr tv tbody) -> keyWordHtml "λ" ++ varHtml v tv ++ showBody
      where showBody = keyWordHtml " . " ++ htmlTypeTree body tbody
  where surroundNonPrim x tp =
          if isPrim x then htmlTypeTree x tp else "(" ++ htmlTypeTree x tp ++ ")"
        surroundAbstr l@(Abstr _ _) tp = "(" ++ htmlTypeTree l tp ++ ")"
        surroundAbstr other tp = htmlTypeTree other tp

inferConstraintHtml :: Term -> TyConstraintTree -> Html
inferConstraintHtml term ctree =
  case inferTypeWithConstriant term ctree of
    Left msg -> msg
    Right (tp, tpTree) -> htmlTypeTree term tpTree ++ " : " ++ typeHtml tp

mkPage :: String -> Html -> Html
mkPage title body =
  concat ["<!DOCTYPE html>\n",
    "<html lang='en'>\n<head>\n<title>", title, "</title>",
    "<meta charset='utf-8'>",
    "<link rel='stylesheet' type='text/css' href='style.css'>",
    "\n</head>",
    "\n<body>",body,"</body></html>"]
