module MetSortTest where
import CoreS.Parse
import CoreS.AST
import MethodSort

main :: IO()
main =  do
  s <- readFile "C:/Users/ak/Documents/GitHub/DATX02-17-26/Test/MethodSortTest/Good1.java"
  s' <- readFile "C:/Users/ak/Documents/GitHub/DATX02-17-26/Test/MethodSortTest/Good1-1.java"
  let ast = parseConv s
  let ast2 = parseConv s'
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
