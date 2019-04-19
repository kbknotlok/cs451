import System.Random

--Main function to start program and prompt for usernames
main = do
     putStrLn "Enter p1 username: "
     p1 <- getLine
     putStrLn "Enter p2 username: "
     p2 <- getLine

     putStrLn (p1 ++ ", choose your class: ")
     putStrLn ("(1) Knight -> Better health, lower hit probability")
     putStrLn ("(other) Ranger -> Lower Health, higher hit probability")
     p1Class <- getLine

     putStrLn (p2 ++ ", choose your class: ")
     putStrLn ("(1) Knight -> Better health (75), lower hit probability (-3)")
     putStrLn ("(other) Ranger -> Lower Health (50), higher hit probability (+3)")
     p2Class <- getLine
     
     let 

     if p1Class == "1" then do
              let p1Stats = (75, 8)
              putStrLn (p1 ++ " has chosen the knight class! May your sword serve you well!\n")
     else do
              let p1Stats = (50, 5)
              putStrLn (p1 ++ " has chosen the ranger class! Your accuracy in battle precedes you!\n")

     if p2Class == "1" then do
              let p2Stats = (75, 8)
              putStrLn (p2 ++ " has chosen the knight class! May your sword serve you well!\n")
     else do
              let p2Stats = (50, 5)
              putStrLn (p2 ++ " has chosen the ranger class! Your accuracy in battle precedes you!\n")

     putStrLn ("\nIt looks as though our matchup is " ++ p1 ++ " vs " ++ p2 ++ "... LET THE GAMES BEGIN!!!")
     gen <- getStdGen
     loop p1 p2 p1Stats p2Stats gen

--Player 2 wins
loop p1 p2 (0, p1Hit) p2Stats seed = putStrLn ("\n\nLadies and genlemen, meet your champion: " ++ p2)

--Player 1 wins
loop p1 p2 p1Stats (0, p2Hit) seed = putStrLn ("\n\nLadies and gentlemen, meet your champion: " ++ p1)

--Main game loop
loop p1 p2 p1Stats p2Stats seed = do

     let p1Health = (fst p1Stats)
     let p1Hit = (snd p1Stats)

     let p2Health = (fst p2Stats)
     let p2Hit = (snd p2Stats)

     --Player 1 turn
     putStrLn ("\n\n\n" ++ p1 ++ ", it is your turn.")
     putStrLn ("Choose an option: ")
     putStrLn ("1 = Slash (High damage, low probability of hit)")
     putStrLn ("2 = Punch (Low damage, high probability of hit)")
     putStrLn ("3 = Kick (Medium damage, medium probability of hit)")
     putStrLn ("other = Roll for potion (Heal)\n\n")
     p1Choice <- getLine
     
     result <- executeChoice p1Choice p1Hit seed --Execute player 1 choice and return result

     let r1 = (fst result)
     let r2 = (snd result)
     
     if r1 > 0 then do
                      p1Health <- return (p1Health + r1)
                      putStrLn ("Nice! Your luck payed off and you gained 10 hp!")
     else if r1 < 0 then do
                           p2Health <- return (p2Health + r1)
                           putStrLn ("Your attack just landed for " ++ (show r1) ++ " damage!")
                           putStrLn (p2 ++ " health is now " ++ (show p2Health))
     else do
        if p1Choice == "1" || p1Choice == "2" || p1Choice == "3" then do
                       putStrLn ("Bummer! Your attack missed and did 0 damage!")
                       putStrLn (p2 ++ " health is still " ++ (show p2Health))
        else do
           putStrLn ("Your one unlucky fella! You did NOT roll for a potion!")
           putStrLn ("Your health remains at " ++ (show p1Health))
     
     --Player 2 turn
     putStrLn ("\n\n\n" ++ p2 ++ ", it is your turn.")
     putStrLn ("Choose an option: ")
     putStrLn ("1 = Slash (High damage, low probability of hit)")
     putStrLn ("2 = Punch (Low damage, high probability of hit)")
     putStrLn ("3 = Kick (Medium damage, medium probability of hit)")
     putStrLn ("other = Roll for potion (Heal)\n\n")
     p2Choice <- getLine
     
     result <- executeChoice p2Choice p2Hit r2 --Execute player 2 choice and return result

     let r1 = (fst result)
     let r2 = (snd result)
     
     if r1 > 0 then do
                      p2Health <- return (p2Health + r1)
                      putStrLn ("Nice! Your luck payed off and you gained 10 hp!")
     else if r1 < 0 then do
                           p1Health <- return (p1Health + r1)
                           putStrLn ("Your attack just landed for " ++ (show r1) ++ " damage!")
                           putStrLn (p1 ++ " health is now " ++ (show p1Health))
     else do
        if p2Choice == "1" || p2Choice == "2" || p2Choice == "3" then do
                       putStrLn ("Bummer! Your attack missed and did 0 damage!")
                       putStrLn (p1 ++ " health is still " ++ (show p1Health))
        else do
           putStrLn ("Your one unlucky fella! You did NOT roll for a potion!")
           putStrLn ("Your health remains at " ++ (show p2Health))

     loop p1 p2 (p1Health, p1Hit) (p2Health, p2Hit) r2 --Loop again

   
--Execute the choice picked by current player
executeChoice :: [Char] -> Integer -> StdGen -> IO (Int, StdGen)
executeChoice choice hit seed
     | choice == "1" = tryASlash hit seed
     | choice == "2" = tryAPunch hit seed
     | choice == "3" = tryAKick hit seed
     | otherwise = tryAPotion hit seed

--Try a slash attack
--Returns the hit value and the next seed for randomization
tryASlash :: Integer -> StdGen -> IO (Int, StdGen)
tryASlash hit seed = do
            gen <- getStdGen
            let tup = randomR(1, (4 + hit)) seed :: (Int, StdGen)
            if (fst tup) == 1 then return (-10, (snd tup))
            else return (0, (snd tup))

--Try a punch attack
--Returns the hit value and the next see for randomization
tryAPunch :: Integer -> StdGen -> IO (Int, StdGen)
tryAPunch hit seed = do
            gen <- getStdGen
            let tup = randomR(1, hit) seed :: (Int, StdGen)
            print(fst tup)
            if (fst tup) == 1 then return (-2, (snd tup))
            else return (0, (snd tup))

--Try a kick attack
--Returns the hit value and the next seed for randomization
tryAKick :: Integer -> StdGen -> IO (Int, StdGen)
tryAKick hit seed = do
            gen <- getStdGen
            let tup = randomR(1, (2 + hit)) seed :: (Int, StdGen)
            if (fst tup) == 1 then return (-5, (snd tup))
            else return (0, (snd tup))

--Try to roll for a potion
--Returns the heal value and the next seed for randomization
tryAPotion :: Integer -> StdGen -> IO (Int, StdGen)
tryAPotion hit seed = do
            gen <- getStdGen
            let tup = randomR(1, 10) seed :: (Int, StdGen)
            if (fst tup) == 1 then return (10, (snd tup))
            else return (0, (snd tup))
 
