module Lang.Gen.PrettyPrinter where

import Text.PrettyPrint
import Lang.Gen.AST
import Lang.PPDoc



ppAST :: OuterExpression -> PPDoc
ppAST = ppOuterExpression

ppOuterExpression :: OuterExpression -> PPDoc
ppOuterExpression (OuterExpression pos a0) = (posToken pos)
       ~~  (ppExp a0)

ppExp :: Exp -> PPDoc
ppExp (Addition pos a0 a1) = (posToken pos)
       ~~  (ppExp a0)
       ~+~ (stringToken "+")
       ~+~ (ppExp a1)
ppExp (Subtraction pos a0) = (posToken pos)
       ~~  (ppMaybe a0 ppGroup)
ppExp (Multiplication pos a0 a1) = (posToken pos)
       ~~  (ppExp a0)
       ~+~ (stringToken "*")
       ~+~ (ppExp a1)
ppExp (Division pos a0) = (posToken pos)
       ~~  (ppMaybe a0 ppGroup0)
ppExp (Number pos a0) = (posToken pos)
       ~~  (ppnumber a0)
ppExp (Parentheses pos a0) = (posToken pos)
       ~~  (stringToken "(")
       ~+~ (ppExp a0)
       ~+~ (stringToken ")")

ppGroup :: Group -> PPDoc
ppGroup (Group pos a0 a1) = (posToken pos)
       ~~  (ppExp a0)
       ~+~ (stringToken "-")
       ~+~ (ppExp a1)

ppGroup0 :: Group0 -> PPDoc
ppGroup0 (Group0 pos a0 a1) = (posToken pos)
       ~~  (ppExp a0)
       ~+~ (stringToken "/")
       ~+~ (ppExp a1)

ppnumber :: (Pos, Int) -> PPDoc
ppnumber (pos,a) = lexToken pos ((show) a)         
