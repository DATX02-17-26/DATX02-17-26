module ForNormTest where
import CoreS.Parse
import CoreS.AST
import NormFor

main :: IO()
main =  do
  s <- readFile "C:\\Users\\ak\\Documents\\GitHub\\DATX02-17-26\\Test\\ForToWhile\\Test1.java"
  s' <- readFile "C:\\Users\\ak\\Documents\\GitHub\\DATX02-17-26\\Test\\ForToWhile\\Test2.java"
  let ast = parseConvUnit s
  let ast2 = parseConvUnit s'
  student <- either undefined pure ast
  model <- either undefined pure ast2
  comapareAST student model

comapareAST :: CompilationUnit -> CompilationUnit -> IO()
comapareAST student model = case execute student of
  Just a -> if a == model
    then print $ "The model is equal to the student"
    else do print $ "The student: " ++ (show a)
            print ("The modelso: " ++ (show model))
  Nothing -> print $ "Nothing done " ++  show student
