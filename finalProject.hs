main = start

start = do
    putStrLn "Would you like to draw a card, flip a coin, or roll a die? (1 -> card, 2 -> coin, 3 <- roll die, 4 <- quit)"
    choice <- getLine
    if choice == "1" then
        drawCard
    else if choice == "2" then
        flipCoin
    else if choice == "3" then
        rollDice
    else if choice == "4" then
        putStrLn "Good bye."
    else do
        putStrLn "Not a valid choice!"
        start

drawCard = do
    putStrLn "Pick a card or suit. (1 <- card, 2 <- suit)"
    ch <- getLine
    if ch == "1" then do
        putStrLn "Pick what card (A, 1 .... , J, Q, K)"
        card <- getLine
        putStrLn "How many of that card would you like to draw? (1..4)"
        num <- getLine
        putStrLn "Would you like to have replacement (1 <- yes, 2 <- no)"
        rep <- getLine
        if rep == "1" then do
            putStrLn ("The probability of drawing " ++ num ++ " " ++ card ++ "'s with replacement is " ++ (show (probab (read num :: Int) 4 52 "Rep")) ++ "%")
        else do
            putStrLn ("The probability of drawing " ++ num ++ " " ++ card ++ "'s without replacement is " ++ (show (probab (read num :: Int) 4 52 "NoRep")) ++ "%")
    else if ch == "2" then do
        putStrLn "Pick what suit (Diamonds, Clubs, Hearts, Spades)"
        suit <- getLine
        putStrLn "How many of that suit would you like to draw? (1..13)"
        numb <- getLine
        putStrLn "Would you like to have replacement (1 <- yes, 2 <- no)"
        repl <- getLine
        if repl == "1" then do
            putStrLn ("The probability of drawing " ++ numb ++ " " ++ suit ++ " with replacement is " ++ (show (probab (read numb :: Int) 13 52 "Rep")) ++ "%")
        else do
            putStrLn ("The probability of drawing " ++ numb ++ " " ++ suit ++ " without replacement is " ++ (show (probab (read numb :: Int) 13 52 "NoRep")) ++ "%")
    else do 
        putStrLn "Not a valid choice"
        drawCard
    start

rollDice = do
    putStrLn "How many sides does the die have?"
    sides <- getLine
    putStrLn "How many times do you want to roll the die?"
    times <- getLine
    putStrLn "What number are you trying to roll?"
    n <- getLine
    putStrLn ("The probability of getting a " ++ n ++ " " ++ times ++ " times is " ++ (show (probab (read times) 1 (read sides) "Rep")) ++ "%")
    start

flipCoin = do
    putStrLn "Type out a series of H's and T's you'd like to simulate (ex: HH -> .25%)"
    series <- getLine
    putStrLn ("The probability of getting " ++ series ++ " in that exact same order is " ++ (show (probab (length series) 1 2 "Rep")) ++ "%")
    start


probab :: Int -> Float -> Float -> String -> Float
probab w x y z = 
    if z == "Rep" then
        getProb (take w (repeat (x/y)))
    else
        getProb [((x-(fromIntegral k))/(y-(fromIntegral k))) | k <- [0..(w-1)]]


getProb :: [Float] -> Float
getProb xs = (product (xs)) * 100
