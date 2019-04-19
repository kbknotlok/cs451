import System.Random

--Main function to start program and prompt for usernames
main = do
          putStrLn "Enter p1 username: "
          p1 <- getLine
          putStrLn "Enter p2 username: "
          p2 <- getLine
          putStrLn ("\nIt looks as though our matchup is " ++ p1 ++ " vs " ++ p2 ++ "... LET THE GAMES BEGIN!!!")
          gen <- getStdGen
          loop p1 p2 50 50 gen

loop p1 p2 0 p2hp seed = putStrLn ("\n\nLadies and genlemen, meet your champion: " ++ p2)

loop p1 p2 p1hp 0 seed = putStrLn ("\n\nLadies and gentlemen, meet your champion: " ++ p1)

loop p1 p2 p1hp p2hp seed = do
     putStrLn ("\n\n\n" ++ p1 ++ ", it is your turn.")
     putStrLn ("Choose an option: ")
     putStrLn ("1 = Slash (High damage, low probability of hit)")
     putStrLn ("2 = Punch (Low damage, high probability of hit)")
     putStrLn ("3 = Kick (Medium damage, medium probability of hit)")
     putStrLn ("other = Roll for potion (Heal)\n\n")
     p1Choice <- getLine
     
     result <- executeChoice p1Choice seed

     let r1 = (fst result)
     let r2 = (snd result)
     
     if r1 > 0 then do
                      newP1hp <- return (p1hp + r1)
                      putStrLn ("Nice! Your luck payed off and you gained 10 hp!")
     else if r1 < 0 then do
                           p2hp <- return (p2hp + r1)
                           putStrLn ("Your attack just landed for " ++ (show r1) ++ " damage!")
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
     
     result <- executeChoice p2Choice seed

     let r1 = (fst result)
     let r2 = (snd result)
     
     if r1 > 0 then do
                      p2hp <- return (p2hp + r1)
                      putStrLn ("Nice! Your luck payed off and you gained 10 hp!")
     else if r1 < 0 then do
                           p1hp <- return (p1hp + r1)
                           putStrLn ("Your attack just landed for " ++ (show r1) ++ " damage!")
                           putStrLn (p1 ++ " health is now " ++ (show p1hp))
     else do
        if p2Choice == "1" || p2Choice == "2" || p2Choice == "3" then do
                       putStrLn ("Bummer! Your attack missed and did 0 damage!")
                       putStrLn (p1 ++ " health is still " ++ (show p1hp))
        else do
           putStrLn ("Your one unlucky fella! You did NOT roll for a potion!")
           putStrLn ("Your health remains at " ++ (show p2hp))

     loop p1 p2 p1hp p2hp r2    

   

executeChoice :: [Char] -> StdGen -> IO (Int, StdGen)
executeChoice choice seed
     | choice == "1" = tryASlash seed
     | choice == "2" = tryAPunch seed
     | choice == "3" = tryAKick seed
     | otherwise = tryAPotion seed


tryASlash :: StdGen -> IO (Int, StdGen)
tryASlash seed = do
            gen <- getStdGen
            let tup = randomR(0, 10) seed :: (Int, StdGen)
            if (fst tup) == 1 then return (-10, (snd tup))
            else return (0, (snd tup))

tryAPunch :: StdGen -> IO (Int, StdGen)
tryAPunch seed = do
            gen <- getStdGen
            let tup = randomR(0, 10) seed :: (Int, StdGen)
            if (fst tup) == 1 then return (-2, (snd tup))
            else return (0, (snd tup))

tryAKick :: StdGen -> IO (Int, StdGen)
tryAKick seed = do
            gen <- getStdGen
            let tup = randomR(0, 10) seed :: (Int, StdGen)
            if (fst tup) == 1 then return (-5, (snd tup))
            else return (0, (snd tup))

tryAPotion :: StdGen -> IO (Int, StdGen)
tryAPotion seed = do
            gen <- getStdGen
            let tup = randomR(0, 10) seed :: (Int, StdGen)
            if (fst tup) == 1 then return (10, (snd tup))
            else return (0, (snd tup))
 
