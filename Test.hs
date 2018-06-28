{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

import Language.Haskell.TH
import Data.Maybe
import Control.Monad.Writer
import Development.Shake
import Data.Generics.Uniplate.Data
import Data.Foldable
import Lib

main :: IO ()
main = do
  forM_ [0 .. 30] $ \ i -> do
    term <- unType <$> runQ (factorial i)
    putStrLn ("i: " ++ show i)
    putStrLn "=== unsimplified: ====="
    test $ toJs term
    putStrLn "======================="
    putStrLn "=== simplified: ======="
    test $ toJs $ simplify term
    putStrLn "======================="

test :: String -> IO ()
test code = do
  putStrLn code
  writeFile "foo.js" (code ++ "\nconsole.log(f(3.1))\n")
  unit $ cmd "node foo.js"

toJs :: Exp -> String
toJs exp = case exp of
  (LamE [pattern] body) | show pattern == "ConP GHC.Tuple.() []" ->
    let ((returnExpression, typ), letClauses) = runWriter $ inner body
    in toFunction (Just "f") letClauses returnExpression typ
  _ -> error ("toJs: " ++ show exp)

indent :: [String] -> [String]
indent = map ("  " ++) . concat . map lines

toFunction :: Maybe String -> [String] -> String -> ExprType -> String
toFunction name letClauses returnExpression typ = unlines $
  ("function " ++ fromMaybe "" name ++ "() {") :
  indent (renderBody letClauses returnExpression typ) ++
  "}" :
  []

renderBody :: [String] -> String -> ExprType -> [String]
renderBody letClauses returnExpression typ =
  letClauses ++
  returnStatement :
  []
  where
    returnStatement = case typ of
      Expression -> "return " ++ returnExpression ++ ";"
      Statement -> returnExpression

data ExprType = Statement | Expression

inner :: Exp -> Writer [String] (String, ExprType)
inner x = case x of
  LamE [ConP unit []] body | show unit == "GHC.Tuple.()" -> do
    let ((returnExpression, typ), letClauses) = runWriter $ inner body
    return (toFunction Nothing letClauses returnExpression typ, Expression)
  VarE v -> return (show v, Expression)
  LitE (IntegerL i) -> return (show i, Expression)
  InfixE (Just left) operator (Just right)
    | show operator == "VarE GHC.Num.*" -> do
      (l, Expression) <- inner left
      (r, Expression) <- inner right
      return (l ++ " * " ++ r, Expression)
  InfixE (Just left) operator (Just right)
    | show operator == "VarE GHC.Classes.==" -> do
      (l, Expression) <- inner left
      (r, Expression) <- inner right
      return (l ++ " == " ++ r, Expression)
  InfixE (Just left) operator (Just right)
    | show operator == "VarE GHC.Real./" -> do
      (l, Expression) <- inner left
      (r, Expression) <- inner right
      return (l ++ " / (" ++ r ++ ")", Expression)
  AppE f (ConE unit) | show unit == "GHC.Tuple.()" -> do
    (g, Expression) <- inner f
    return (g ++ "()", Expression)
  AppE f x -> do
    (g, Expression) <- inner f
    (y, Expression) <- inner x
    return (g ++ "(" ++ y ++ ")", Expression)
  LetE [ValD (VarP l) (NormalB lExpr) []] expr -> do
    (l_e, Expression) <- inner lExpr
    tell ["let " ++ show l ++ " = " ++ l_e ++ ";"]
    inner expr
  CondE cond t e -> do
    (condE, Expression) <- inner cond
    let ((tE, tTyp), tLetClauses) = runWriter $ inner t
    let ((eE, eTyp), eLetClauses) = runWriter $ inner e
    return $ (, Statement) $ unlines $
      ("if (" ++ condE ++ ") {") :
      indent (renderBody tLetClauses tE tTyp) ++
      "} else {" :
      indent (renderBody eLetClauses eE eTyp) ++
      "}" :
      []
  x -> error $ ("inner: " ++ show x)

simplify :: Exp -> Exp
simplify = rewrite $ \ ast -> case ast of
  -- beta reduction
  AppE (LamE [VarP p] body) x -> Just $ replace p x body
  AppE (LamE [ConP unit []] body) (ConE unit2)
    | show unit == "GHC.Tuple.()" &&
      show unit2 == "GHC.Tuple.()"
      -> Just body
  _ -> Nothing

replace :: Name -> Exp -> Exp -> Exp
replace name insertion = rewrite $ \ ast -> case ast of
  VarE var ->
    if name == var
      then Just insertion
      else Nothing
  _ -> Nothing
