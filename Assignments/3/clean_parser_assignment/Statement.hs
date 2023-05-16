module Statement (T, parse, toString, fromString, exec) where

import Data.Monoid (Endo)
import Data.Text.Internal.Fusion (Step (Skip))
import Dictionary qualified
import Expr qualified
import GHC.Stg.Lift.Monad (FloatLang (EndBindingGroup))
import Parser hiding (T)
import Prelude hiding (fail, return)

type T = Statement

data Statement
  = Assignment String Expr.T
  | If Expr.T Statement Statement
  | Skip
  | Begin [Statement]
  | While Expr.T Statement
  | Read String
  | Write Expr.T
  deriving (Show)

-- statement ::= variable ':=' expr ';'
--            | 'skip' ';'
--            | 'begin' statements 'end'
--            | 'if' expr 'then' statement 'else' statement
--            | 'while' expr 'do' statement
--            | 'read' variable ';'
--            | 'write' expr ';'

assStatement = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

skipStatement = accept "skip" # require ";" >-> const Statement.Skip

beginStatement = accept "begin" -# iter parse #- require "end" >-> \xs -> Begin xs

ifStatement = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> \((e, x), y) -> If e x y

buildAss (v, e) = Assignment v e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts : stmts) dict input =
  if (Expr.value cond dict) > 0
    then exec (thenStmts : stmts) dict input
    else exec (elseStmts : stmts) dict input

instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"
