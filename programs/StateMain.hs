module StateMain where

type Stack :: [Int]

main :: IO()
main = do
      let type Stack :: [Int]

          pop :: Stack -> (Int, Stack)
          pop (x:xs) = (x, xs)
