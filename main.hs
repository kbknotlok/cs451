import InputModule
import ComputationModule

main = do medium <- getMedium
          drawNum <- getDrawNum
          let intDrawNum = read drawNum :: Integer
          putStrLn (medium ++ " and " ++ drawNum)
          putStrLn "Hi"
          let cardList = [1,2,3]
          run intDrawNum
