module CoreParser(Parser, char, return, fail, (#), (!), (?), (#>), (>->),
                  Parse, parse, toString, fromString) where
import Prelude hiding (return, fail)
infixl 3 ! 
infixl 7 ?
infixl 6 #
infixl 5 >->
infixl 4 #>

class Parse a where
    parse :: Parser a
    fromString :: String -> a
    fromString cs =
        case parse cs of
               Just(s, []) -> s
               Just(s, cs) -> error ("garbage '"++cs++"'")
               Nothing -> error "Nothing"
    toString :: a -> String

type Parser a = String -> Maybe (a, String)

char :: Parser Char
char []= Nothing
char (c:cs) = Just (c, cs)

return :: a -> Parser a
return a cs = Just (a, cs)

fail ::  Parser a 
fail cs = Nothing

-- uses two different parsers to try and parse the input
(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs = case m cs of
        Nothing -> n cs 
        mcs -> mcs

-- uses one parser and one predicate function and validates the parsed input with the predicate
-- only returning just(r,s) if the predicate is true
(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs = 
        case m cs of
        Nothing -> Nothing
        Just(r, s) -> if p r then Just(r, s) else Nothing

-- uses two parsers to try and parse the input
-- if the first parser fails, the second parser is used
-- if the first parser succeeds, the second parser will also be tried on the rest of the input
-- a is the input we could parse with function m, and b is the input we could parse with fucntion n
(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs = 
    case m cs of
    Nothing -> Nothing
    Just(a, cs') -> 
        case n cs' of
        Nothing -> Nothing
        Just(b, cs'') -> Just((a, b), cs'')

-- takes on parser and a funciton from a to b, first parses the input and the applies the function
(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> b) cs = 
    case m cs of
    Just(a, cs') -> Just(b a, cs')
    Nothing -> Nothing

-- takes on parser and a function that returns a parsed input
-- applies the function to the input if it can be parsed with the first parser
(#>) :: Parser a -> (a -> Parser b) -> Parser b 
(p #> k) cs = 
    case p cs of
    Nothing -> Nothing
    Just(a, cs') -> k a cs'
