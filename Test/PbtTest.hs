module PbtTest where
import PropertyBasedTesting
import EvaluationMonad
import Test.QuickCheck (listOf, choose)

runPbtTest =  executeEvalM testEnv $ runPBT path gen

path = "C:\\\\Workspace\\DATX02-17-26\\Test\\PbtTest"

gen = listOf $ choose ('a','z')

testEnv = Env { verbose = True
   , logfile       = "logfile.log"
   , numberOfTests = 1
   }
