-- Antonio Krsoski & Willard Råborg

module Statement (T, parse, toString, fromString, exec) where

import Data.Monoid (Endo)
import Data.Text.Internal.Fusion (Step ())
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
  | Comment String
  deriving (Show)

-- statement ::= variable ':=' expr ';'
--            | 'skip' ';'
--            | 'begin' statements 'end'
--            | 'if' expr 'then' statement 'else' statement
--            | 'while' expr 'do' statement
--            | 'read' variable ';'
--            | 'write' expr ';'

assStatement = word #- accept ":=" # Expr.parse #- require ";" >-> \(v, e) -> Assignment v e

skipStatement = accept "skip" # require ";" >-> \_ -> Skip

beginStatement = accept "begin" -# iter parse #- require "end" >-> \xs -> Begin xs

ifStatement = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> \((e, x), y) -> If e x y

whileStatement = accept "while" -# Expr.parse # require "do" -# parse >-> \(e, x) -> While e x

readStatement = accept "read" -# word #- require ";" >-> \v -> Read v

writeStatement = accept "write" -# Expr.parse #- require ";" >-> \v -> Write v

commentStatement = accept "--" -# line #- require "\n" >-> \s -> Comment s

indent = flip take (repeat '\t')

shw :: Int -> Statement -> String
shw n (Assignment v e) = indent n ++ v ++ " := " ++ Expr.toString e ++ ";\n"
shw n (Skip) = indent n ++ "skip;\n"
shw n (Begin stmts) = indent n ++ "begin\n" ++ concat (map (shw (n + 1)) stmts) ++ indent n ++ "end\n"
shw n (If cond s1 s2) = indent n ++ "if " ++ Expr.toString cond ++ " then\n" ++ shw (n + 1) s1 ++ indent n ++ "else\n" ++ shw (n + 1) s2
shw n (While cond stmts) = indent n ++ "while " ++ Expr.toString cond ++ " do\n" ++ shw (n + 1) stmts
shw n (Read s) = indent n ++ "read " ++ s ++ ";\n"
shw n (Write s) = indent n ++ "write " ++ Expr.toString s ++ ";\n"
shw n (Comment s) = indent n ++ "-- " ++ s ++ "\n"

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec ((Assignment s e) : stmts) d i = exec stmts (Dictionary.insert (s, Expr.value e d) d) i
exec ((Skip) : stmts) d i = exec stmts d i
exec ((Begin xs) : stmts) d i = exec (xs ++ stmts) d i
exec ((If cond thenStmts elseStmts) : stmts) d i =
  if (Expr.value cond d) > 0
    then exec (thenStmts : stmts) d i
    else exec (elseStmts : stmts) d i
exec ((While cond s) : stmts) d i =
  if (Expr.value cond d) > 0
    then exec (s : (While cond s) : stmts) d i
    else exec stmts d i
exec ((Read s) : stmts) d (i : is) = exec stmts (Dictionary.insert (s, i) d) is
exec ((Write e) : stmts) d i = Expr.value e d : exec stmts d i
exec (Comment _ : stmts) d i = exec stmts d i

instance Parse Statement where
  parse = assStatement ! skipStatement ! beginStatement ! ifStatement ! whileStatement ! readStatement ! writeStatement ! commentStatement
  toString = shw 0
