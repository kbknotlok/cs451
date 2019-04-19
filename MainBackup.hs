import System.Random
--import InputModule
--import ComputationModule

main = do
          putStrLn "Enter p1 username: "
          p1 <- getLine
          putStrLn "Enter p2 username: "
          p2 <- getLine
          putStrLn ("It looks as though our matchup is " ++ p1 ++ " vs " ++ p2 ++ "... LET THE GAMES BEGIN!!!")
          loop p1 p2 50 50

loop p1 p2 p1hp p2hp = do
     putStrLn (p1 ++ ", it is your turn.")
     putStrLn ("Choose an option: ")
     putStrLn ("1 = Slash (High damage, low probability of hit)")
     putStrLn ("2 = Punch (Low damage, high probability of hit)")
     putStrLn ("3 = Kick (Medium damage, medium probability of hit)")
     putStrLn ("other = Roll for potion (Heal)")
     p1Choice <- getLine
     
     let result = executeChoice p1Choice
     print (result)



executeChoice :: [Char] -> Integer
executeChoice choice
     | choice == "1" = tryASlash
     | choice == "2" = tryAPunch
     | choice == "3" = tryAKick
     | otherwise = tryAPotion


tryASlash :: IO Int
tryASlash = do
               gen <- getStdGen
               let (randIndex, _) = randomR(0, 10) gen :: (Int, StdGen)
            if randIndex == 1

tryAPunch :: IO Int
tryAPunch = do
               gen <- getStdGen
               let (randIndex, _) = randomR(0, 10) gen :: (Int, StdGen)

tryAKick :: IO Int
tryAKick = do
               gen <- getStdGen
               let (randIndex, _) = randomR(0, 10) gen :: (Int, StdGen)

tryAPotion :: IO Int
tryAPotion = do
               gen <- getStdGen
               let (randIndex, _) = randomR(0, 10) gen :: (Int, StdGen)
 
