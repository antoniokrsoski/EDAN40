-- Antonio Krsoski & Willard Råborg

module Program (T, parse, fromString, toString, exec) where

import Dictionary qualified
import Parser hiding (T)
import Statement qualified
import Prelude hiding (fail, return)

newtype T = Program [Statement.T] -- to be defined

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program stmts) = concatMap Statement.toString stmts

exec (Program p) = Statement.exec p Dictionary.empty
