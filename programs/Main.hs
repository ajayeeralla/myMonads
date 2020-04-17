module Main where

main :: IO ()
main = do
   let a :: Either String Int
       a = Right 20

       b :: Either String Int
       b = Right 30

       f :: Int -> Int -> Either String Int
       f a b = Right $ a * b

       c :: Either String Int
       c = do
           aVal <- a
           bVal <- b
           f aVal bVal

       x :: [Int]
       x = do
           y <- [1, 2, 3]
           z <- [4, 5, 6]
           return $ y * z

       z :: Maybe Int
       z = Just 20

       g :: Int -> Maybe Int
       g x = Just $ x + 1

       res :: Maybe Int
       res = do
           x <- z
           g x

   --putStrLn $ show x
   print x
   print c
   print res
