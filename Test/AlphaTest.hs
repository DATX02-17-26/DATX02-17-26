module AlphaTest where
import CoreS.Parse
import CoreS.AST
import AlphaR

main :: IO()
main =  do
  s <- readFile "AlphaTest/Good1.java"
  let ast = parseConvUnit s
  ast <- either undefined pure
  compareAST ast

comapareAST :: CompilationUnit -> IO()
comapareAST ast = case execute ast of
  Just a -> return (show a)
  Nothing -> return (show "WTF!")
