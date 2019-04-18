module ComputationModule (run, getCompStats, getContStats) where

run n
  | n == 0 = return ()
  | otherwise = do
                  putStrLn "Testing Loop"
                  run (n - 1)
	
getCompStats = do putStrLn "These are comp stats"
	 
getContStats = do putStrLn "These are cont stats"
