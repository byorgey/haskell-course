
-- putStrLn :: String -> IO ()

main :: IO ()
main = promptSucc

doNTimes :: Int -> IO a -> IO ()
doNTimes n _ | n < 1 = return ()
doNTimes n x = x >> doNTimes (n-1) x

-- return :: a -> IO a


-- x >> y  means "do x, then do y"
-- (>>) :: IO a -> IO b -> IO b
-- "and then"

{-
readLn :: Read a => IO a

read :: Read a => String -> a

(>>=) :: IO a -> (a -> IO b) -> IO b
"bind"

-}

promptSucc :: IO ()
promptSucc = putStrLn "Please enter a number:"
          >> (readLn >>= (\i -> putStrLn (show (i+1))))
