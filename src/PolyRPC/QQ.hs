{-# LANGUAGE TemplateHaskell #-}
module PolyRPC.QQ (rpc) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)

-- Very tiny parser for:  at <ident> { <fun> <argExpr> }
-- No nesting or complex grammar; it's a demo.
rpc :: QuasiQuoter
rpc = QuasiQuoter
  { quoteExp  = qExp
  , quotePat  = err "patterns"
  , quoteType = err "types"
  , quoteDec  = err "declarations"
  }
  where err w = const (fail ("[$rpc| ... |] cannot appear in " ++ w))

qExp :: String -> Q Exp
qExp s0 = do
  let s = trim s0
  case lexAt s of
    Nothing -> fail ("rpc quasiquoter: expected: at <handle> { <fun> <arg> }, got: " ++ show s)
    Just (hname, inside) ->
      case splitFun inside of
        Nothing -> fail ("rpc quasiquoter: body must look like: <fun> <argExpr>, got: " ++ show inside)
        Just (fname, argStr) -> do
          let h = varE (mkName hname)
          let f = varE (mkName fname)
          argExp <- parseArg argStr
          -- callH h f arg
          [| callH $(h) $(f) $(return argExp) |]

-- Parse leading: 'at <ident> { ... }'
lexAt :: String -> Maybe (String, String)
lexAt s =
  let s1 = dropWhile isSpace s
  in case splitWord s1 of
      ("at", rest1) ->
        let (hn, rest2) = splitIdent (dropWhile isSpace rest1)
        in if null hn then Nothing else
             let rest3 = dropWhile isSpace rest2
             in case rest3 of
                 ('{':xs) ->
                   let (body, rest4) = breakBrace 0 xs
                   in case rest4 of
                        ('}':_) -> Just (hn, body)
                        _ -> Nothing
                 _ -> Nothing
      _ -> Nothing

-- Split first word (non-space run)
splitWord :: String -> (String, String)
splitWord xs = let (w, r) = span (not . isSpace) xs in (w, r)

-- Identifier: letter or '_' followed by alphanum or '_'s
splitIdent :: String -> (String, String)
splitIdent xs =
  let p1 c = isLetter c || c == '_'
      p c  = isAlphaNum c || c == '_'
  in case xs of
      (c:cs) | p1 c -> let (w,r) = span p cs in (c:w, r)
      _ -> ("", xs)

-- Find matching '}' for outermost block (assuming we start just after '{')
breakBrace :: Int -> String -> (String, String)
breakBrace n [] = ([], [])
breakBrace n (c:cs)
  | c == '{'  = let (a,b) = breakBrace (n+1) cs in (c:a, b)
  | c == '}' =
      if n == 0 then ([], c:cs)  -- 중괄호를 포함해서 반환
      else let (a,b) = breakBrace (n-1) cs in (c:a, b)
  | otherwise = let (a,b) = breakBrace n cs in (c:a, b)

-- Split '<fun> <argExpr>' (fun must be an identifier), arg is rest (trimmed)
splitFun :: String -> Maybe (String, String)
splitFun s =
  let s' = trim s
      (fn, r) = splitIdent s'
  in if null fn then Nothing else Just (fn, trim r)

parseArg :: String -> Q Exp
parseArg s = do
  -- Simple parsing for demo - handle variables and integer literals
  let s' = trim s
  if all isDigit s' 
    then litE (integerL (read s'))  -- integer literal
    else if all (\c -> isAlphaNum c || c `elem` "_'") s' && isLetter (head s')
      then varE (mkName s')  -- variable name
      else fail ("rpc: unsupported argument expression: " ++ s)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- Import callH from PolyRPC.Handle
-- (We don't need to define it here since it will be in scope at splice site)
