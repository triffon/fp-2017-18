import Control.Monad

main = putStrLn "Hello, world!"

notMain = "Hello, world!"

-- getput = putStrLn $ "Въведохте: " ++ getLine
getput = do
           -- text <- return "Въведохте: "
           let text = "Въведохте: "
           line <- getLine
           putStrLn $ text ++ line

palindrome = do putStr "Моля, въведете палиндром: "
                line <- getLine
                let revLine = reverse line
                if revLine == line then putStrLn "Благодаря!"
                  else do putStrLn $ line ++ " не е палиндром!"
                          palindrome

getInt :: IO Int
getInt = do line <- getLine
            return $ read line

addInts :: IO Int
addInts = do n1 <- getInt
             n2 <- getInt
             return $ n1 + n2

findAverage :: IO Double
findAverage = do putStr "Моля въведете брой числа: "
                 n <- getInt
                 s <- readAndSum n
                 return $ (fromIntegral s) / (fromIntegral n)

readAndSum :: Int -> IO Int
readAndSum 0 = return 0
readAndSum n = do putStr "Моля, въведете число: "
                  x <- getInt
                  s <- readAndSum $ n - 1
                  return $ x + s
