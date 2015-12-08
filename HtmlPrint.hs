module HtmlPrint
    (
      inferConstraintHtml
    ) where

import LambdaTerm
import TypedTerm

type Html = String

varHtml name t = "<span title=\"" ++ name ++ ": " ++ show t ++ "\">" ++ name ++ "</span>"

htmlTypeTree :: Term -> NamedTyTerm -> Html
htmlTypeTree term tree =
  case (term, tree) of
    (Var v, Var t) -> varHtml v t
    (Apply x y, Apply tx ty) -> surroundAbstr x tx ++ " " ++ surroundNonPrim y ty
    (Abstr v body, Abstr tv tbody) -> "λ" ++ varHtml v tv ++ showBody
      where showBody = " . " ++ htmlTypeTree body tbody
  where surroundNonPrim x tp =
          if isPrim x then htmlTypeTree x tp else "(" ++ htmlTypeTree x tp ++ ")"
        surroundAbstr l@(Abstr _ _) tp = "(" ++ htmlTypeTree l tp ++ ")"
        surroundAbstr other tp = htmlTypeTree other tp

inferConstraintHtml :: Term -> TyConstraintTree -> Html
inferConstraintHtml term ctree =
  case inferTypeWithConstriant term ctree of
    Left msg -> msg
    Right (tp, tpTree) -> htmlTypeTree term tpTree ++ " : " ++ show tp
