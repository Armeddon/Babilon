module Main where

import Data.Maybe(fromJust)

babilon :: [String]
babilon = [] : [chr : word | word <- babilon, chr <- ['\0' .. '\255']]

index :: Eq a => a -> [a] -> Maybe Int
index elem lst = helper elem lst 0 where
    helper _ [] _ = Nothing
    helper elem (x:xs) n = if elem == x
        then Just n
        else helper elem xs (n + 1)

main :: IO ()
main = do
    input <- getLine
    case input of
        "search" -> do
            putStrLn "Enter a string to search for"
            value <- getLine
            putStrLn $ show $ fromJust $ index value babilon
            main
        "lookup" -> do
            putStrLn "Enter an index to lookup for"
            value <- getLine
            putStrLn $ show $ (babilon !!) $ (read value :: Int)
            main
        "quit" -> return ()
        _ -> do
            putStrLn "Unknown command!!!"
            putStrLn "Possible command are: search, lookup, quit"
            main

