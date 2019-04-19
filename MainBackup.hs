import System.Random

--Main function to start program and prompt for usernames
main = do
          putStrLn "Enter p1 username: "
          p1 <- getLine
          putStrLn "Enter p2 username: "
          p2 <- getLine
          putStrLn ("\nIt looks as though our matchup is " ++ p1 ++ " vs " ++ p2 ++ "... LET THE GAMES BEGIN!!!")
          loop p1 p2 50 50

loop p1 p2 0 p2hp = putStrLn ("\n\nLadies and genlemen, meet your champion: " ++ p2)

loop p1 p2 p1hp 0 = putStrLn ("\n\nLadies and gentlemen, meet your champion: " ++ p1)

loop p1 p2 p1hp p2hp = do
     putStrLn ("\n\n\n" ++ p1 ++ ", it is your turn.")
     putStrLn ("Choose an option: ")
     putStrLn ("1 = Slash (High damage, low probability of hit)")
     putStrLn ("2 = Punch (Low damage, high probability of hit)")
     putStrLn ("3 = Kick (Medium damage, medium probability of hit)")
     putStrLn ("other = Roll for potion (Heal)\n\n")
     p1Choice <- getLine
     
     result <- executeChoice p1Choice
     
     if result > 0 then do
                      p1hp <- return (p1hp + result)
                      putStrLn ("Nice! Your luck payed off and you gained 10 hp!")
     else if result < 0 then do
                           p2hp <- return (p1hp + result)
                           putStrLn ("Your attack just landed for " ++ (show result) ++ " damage!")
                           putStrLn (p2 ++ " health is now " ++ (show p2hp))
     else do
        if p1Choice == "1" || p1Choice == "2" || p1Choice == "3" then do
                       putStrLn ("Bummer! Your attack missed and did 0 damage!")
                       putStrLn (p2 ++ " health is still " ++ (show p2hp))
        else do
           putStrLn ("Your one unlucky fella! You did NOT roll for a potion!")
           putStrLn ("Your health remains at " ++ (show p1hp))
     

     putStrLn ("\n\n\n" ++ p2 ++ ", it is your turn.")
     putStrLn ("Choose an option: ")
     putStrLn ("1 = Slash (High damage, low probability of hit)")
     putStrLn ("2 = Punch (Low damage, high probability of hit)")
     putStrLn ("3 = Kick (Medium damage, medium probability of hit)")
     putStrLn ("other = Roll for potion (Heal)\n\n")
     p2Choice <- getLine
     
     result <- executeChoice p2Choice
     
     if result > 0 then do
                      p2hp <- return (p1hp + result)
                      putStrLn ("Nice! Your luck payed off and you gained 10 hp!")
     else if result < 0 then do
                           p1hp <- return (p1hp + result)
                           putStrLn ("Your attack just landed for " ++ (show result) ++ " damage!")
                           putStrLn (p1 ++ " health is now " ++ (show p1hp))
     else do
        if p2Choice == "1" || p2Choice == "2" || p2Choice == "3" then do
                       putStrLn ("Bummer! Your attack missed and did 0 damage!")
                       putStrLn (p1 ++ " health is still " ++ (show p1hp))
        else do
           putStrLn ("Your one unlucky fella! You did NOT roll for a potion!")
           putStrLn ("Your health remains at " ++ (show p2hp))

     loop p1 p2 p1hp p2hp     

   

executeChoice :: [Char] -> IO Int
executeChoice choice
     | choice == "1" = tryASlash
     | choice == "2" = tryAPunch
     | choice == "3" = tryAKick
     | otherwise = tryAPotion


tryASlash :: IO Int
tryASlash = do
            gen <- getStdGen
            let (randIndex, _) = randomR(0, 10) gen :: (Int, StdGen)
            if randIndex == 1 then return (-10)
            else return 0

tryAPunch :: IO Int
tryAPunch = do
            gen <- getStdGen
            let (randIndex, _) = randomR(0, 10) gen :: (Int, StdGen)
            if randIndex == 1 then return (-2)
            else return 0

tryAKick :: IO Int
tryAKick = do
            gen <- getStdGen
            let (randIndex, _) = randomR(0, 10) gen :: (Int, StdGen)
            if randIndex == 1 then return (-5)
            else return 0

tryAPotion :: IO Int
tryAPotion = do
            gen <- getStdGen
            let (randIndex, _) = randomR(0, 10) gen :: (Int, StdGen)
            if randIndex == 1 then return 10
            else return 0
 
