{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Control.Monad.Writer
import Development.Shake
import Data.Generics.Uniplate.Data
import Data.Foldable
import Lib

main :: IO ()
main = do
  forM_ (take 100 ([0 .. 10] ++ [-1, -2 .. -10])) $ \ i -> do
    term <- unType <$> runQ (power i)
    putStrLn ("i: " ++ show i)
    putStrLn "=== unsimplified: ===="
    test $ to_js term
    putStrLn "======================"
    putStrLn "=== simplified: ==="
    test $ to_js $ simplify term
    putStrLn "======================"

test :: String -> IO ()
test code = do
  putStrLn code
  writeFile "foo.js" (code ++ "\nconsole.log(f(3.1))\n")
  unit $ cmd "node foo.js"

to_js :: Exp -> String
to_js (LamE [VarP a] body) =
  let (returnExpression, letClauses) = runWriter $ inner body
  in unlines $
    ("function f(" ++ show a ++ ") {") :
    map ("  " ++) letClauses ++
    ("  return " ++ returnExpression ++ ";") :
    "}" :
    []


inner :: Exp -> Writer [String] String
inner x = case x of
  LamE [VarP a] body -> do
    let (returnExpression, letClauses) = runWriter $ inner body
    return $ unlines $
      ("function f(" ++ show a ++ ") {") :
      map ("  " ++) letClauses ++
      ("  return " ++ returnExpression ++ ";") :
      "}" :
      []
  VarE v -> return (show v)
  LitE (IntegerL i) -> return (show i ++ ".0")
  InfixE (Just left) operator (Just right)
    | show operator == "VarE GHC.Num.*" -> do
      l <- inner left
      r <- inner right
      return (l ++ " * " ++ r)
  AppE f x -> do
    g <- inner f
    y <- inner x
    return (g ++ "(" ++ y ++ ")")
  LetE [ValD (VarP l) (NormalB l_expr) []] expr -> do
    l_e <- inner l_expr
    tell ["let " ++ show l ++ " = " ++ l_e ++ ";"]
    inner expr
  x -> error $ ("inner: " ++ show x)

simplify :: Exp -> Exp
simplify = rewrite $ \ ast -> case ast of
  AppE (LamE [VarP p] body) x -> Just $ replace p x body
  _ -> Nothing

replace :: Name -> Exp -> Exp -> Exp
replace name insertion = rewrite $ \ ast -> case ast of
  VarE var ->
    if name == var
      then Just insertion
      else Nothing
  _ -> Nothing
