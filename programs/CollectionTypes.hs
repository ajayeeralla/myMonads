module CollectionTypes where
import Data.Map
--import Data.HashMap
--import Data.HashSet
--import Data.Set
--import Data.Sequence

lettersDict = fromList [(5,"a"), (3,"b")]


returnLetter :: Int -> Maybe String
returnLetter c = do
    Data.Map.lookup c lettersDict



returnViewKey :: Map Int String -> Maybe ((Int, String), Map Int String)
returnViewKey c = do
    minViewWithKey lettersDict

main = do
    putStrLn $ "letter with key value 26: " ++ (show (returnLetter 3))
    putStrLn $ "letter with key value 25: " ++ (show (returnViewKey lettersDict))
