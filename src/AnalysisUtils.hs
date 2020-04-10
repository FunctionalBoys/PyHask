module AnalysisUtils where

import           ParserTypes

--TODO: Actually do this
exprCheck :: SimpleExpr -> Parser Expr
exprCheck e = return $ Expr e (Simple IntType)
