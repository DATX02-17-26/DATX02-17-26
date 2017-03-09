module ForNormTest where
import CoreS.Parse
import CoreS.AST
import NormFor

main :: IO()
main =  do
  s <- readFile "ForToWhile/Test1.java"
  s' <- readFile "ForToWhile/Test2.java"
  let ast = parseConvUnit s
  let ast2 = parseConvUnit s'
  student <- either undefined pure ast
  model <- either undefined pure ast2
  comapareAST student model

comapareAST :: CompilationUnit -> CompilationUnit -> IO()
comapareAST student model = case execute student of
  Just a -> if a == model
    then print $ "The model is equal to the student"
    else do print $ "The student solution: " ++ (show a)
            print ("The model: " ++ (show model))
  Nothing -> print $ "WTF!"
