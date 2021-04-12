module Parser where

import Data.List (isPrefixOf, findIndex)
import Data.Char (isDigit, isAlpha, isAlphaNum)
import LCExpr

--  LC	 	=  num
-- 	 	|	 	id
-- 	 	|	 	(/ id => LC)
-- 	 	|	 	(LC LC)
-- 	 	|	 	(+ LC LC)
-- 	 	|	 	(* LC LC)
-- 	 	|	 	(ifleq0 LC LC LC)
-- 	 	|	 	(println LC)

-- Strips leading spaces from a string
consumeLeadingWS :: String -> String
consumeLeadingWS chars
   | null chars = []
   | head chars == ' ' = consumeLeadingWS (tail chars)
   | otherwise = chars

-- reads chars and returns (id,rest)
readID :: String -> (String, String)
readID chars
   | null chars = ([], [])
   | isAlphaNum (head chars) = (head chars:rest_of_id,remaining_chars)
   | otherwise = ([],chars)
   where (rest_of_id,remaining_chars) = readID (tail chars)

-- reads chars and returns (number_as_string, rest)
readNum :: String -> (String, String)
readNum chars
   | null chars = ([], [])
   | isDigit (head chars) || head chars == '-' = (head chars:rest_of_num,remaining_chars)
   | otherwise = ([],chars)
   where (rest_of_num,remaining_chars) = readNum (tail chars)

parseSubExpr :: String -> (Expr,String)
parseSubExpr chars
   | isPrefixOf "(/ " chars =
      let (id,rest) = readID(consumeLeadingWS (drop 3 chars)) in
         -- consume the arrow
         let rest' = drop 2 (consumeLeadingWS rest) in
            let (exp,rest'') = parseExp (consumeLeadingWS rest') in
               (Abs id exp, rest'')
   | isPrefixOf "(+ " chars =
      let (exp1,rest) = parseExp (consumeLeadingWS (drop 3 chars)) in
         let (exp2,rest') = parseExp (consumeLeadingWS rest) in
            (Add exp1 exp2, rest')
   | isPrefixOf "(* " chars = 
      let (exp1,rest) = parseExp (consumeLeadingWS (drop 3 chars)) in
         let (exp2,rest') = parseExp (consumeLeadingWS rest) in
            (Mult exp1 exp2, rest')
   | isPrefixOf "(ifleq0 " chars =
      let (exp1,rest) = parseExp (consumeLeadingWS (drop 3 chars)) in
         let (exp2,rest') = parseExp (consumeLeadingWS rest) in
            let (exp3,rest'') = parseExp (consumeLeadingWS rest') in
               (Ifleq0 exp1 exp2 exp3, rest'')
   | isPrefixOf "(println " chars =
      let (expr,rest) = parseExp (consumeLeadingWS (drop 9 chars)) in
         (Println expr, rest)
   -- Otherwise, it has to be an application: (LC LC)
   | otherwise = 
      let (exp1,rest) = parseExp (consumeLeadingWS (drop 3 chars)) in
         let (exp2,rest') = parseExp (consumeLeadingWS rest) in
            (App exp1 exp2, rest')

parseExp :: String -> (Expr,String)
parseExp chars
   | null chars = (Num "parseExp:ERROR: No characters to parse",[])
   | head chars == '(' =
         let (subexpr,rest) = parseSubExpr chars in
            (subexpr, tail (consumeLeadingWS rest))
   | isDigit (head chars) || head chars == '-' = let (numstring,rest) = readNum chars in (Num numstring, consumeLeadingWS rest)
   | isAlpha (head chars) = let (idstring,rest) = readID chars in (Id idstring, consumeLeadingWS rest)
   | otherwise = (Num ("parseExp:ERROR: Neither a paren, digit, or alpha in : " ++ chars),[])

scanTo :: (String,String) -> (String, String)
scanTo (chars, stop_chars)
   | null chars = ([],[])
   | otherwise =
      -- Find the first occurrence of a space or paren
      let result = findIndex (\c -> c elem stop_chars) (head chars) in
         case result of
            Nothing -> (chars,"") -- No special char found, so return whole string
            Just pos -> splitAt pos chars  

data SexprElem = Str String | Sub Sexpr 
newtype Sexpr = Elem [SexprElem]

parseSexpr :: String -> (Sexpr, String)
parseSexpr chars
   | null chars = (Elem [],[])
   | head chars == ' ' = parseSexpr (tail chars)
   | head chars == ')' = (Elem [Str ""], tail chars)
   | head chars == '(' =
      let (subexp,rest) = parseSexpr (tail chars) in
        (Elem ) 