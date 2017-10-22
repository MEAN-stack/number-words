import Test.QuickCheck
import Data.List
import Data.Char
import qualified Data.Map as Map
import Data.Maybe

numberWordList :: [(Int,String)]
numberWordList = [(0,""),(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five"),(6,"six"),(7,"seven"),(8,"eight"),(9,"nine"),(10,"ten"),(11,"eleven"),(12,"twelve"),(13,"thirteen"),(14,"fourteen"),(15,"fifteen"),(16,"sixteen"),(17,"seventeen"),(18,"eightteen"),(19,"nineteen"),(20,"twenty"),(30,"thirty"),(40,"forty"),(50,"fifty"),(60,"sixty"),(70,"seventy"),(80,"eighty"),(90,"ninety")]

-- association lists
-- map numbers to words
numberWords :: Map.Map Int String
numberWords = Map.fromList numberWordList
-- map words to numbers
wordNumbers :: Map.Map String Int
wordNumbers = Map.fromList $ map (\(x,y) -> (y,x)) numberWordList

--------------------------------------------------------------------------------------------------------
--
-- convert a number to a string of words
--
-- deal with special cases here
-- 
toWords  :: Int -> String
toWords 1000000000 = "one billion"
toWords 0 = "zero"

-- clean up multiple or trailing spaces
--
toWords n = concat $ intersperse " " $ words $ toWords' n

-- workhorse function
--
toWords' :: Int -> String
toWords' n
  | n>=1000000000 = "too big"
  | n>=1000000 = (toWords' $ n `div` 1000000) ++ " million " ++ (toWords' $ n `mod` 1000000)
  | n>=1000 = (toWords' $ n `mod` 1000000 `div` 1000) ++ " thousand " ++ (toWords' $ n `mod` 1000)
  | n>=100 = (toWords' $ n `div` 100) ++ " hundred " ++ (toWords' $ n `mod` 100)
  | n>=20 = let units = (n `mod` 10) in (toWords'' (n - units)) ++ " " ++ (toWords' units)
  | otherwise = toWords'' n

toWords'' :: Int -> String
toWords'' n = if (Map.lookup n numberWords) /= Nothing then fromJust (Map.lookup n numberWords) else ""

--------------------------------------------------------------------------------------------------------
--
-- convert a string of words to a number
-- tokenize the original string, converting to lower case
-- and discarding occurrences of "and"
--
toNumber :: String -> Int
toNumber s = toNumber' (filter (/="and") $ words $ map toLower s)

toNumber' :: [String] -> Int
-- special case
toNumber' ["one", "billion"] = 1000000000
-- split the string into three parts:
--   the number of "millions"
--   the number of "thousands"
--   the remainder
-- each of these comprises hundreds, tens and units
toNumber' ws = 1000000 * (htuToNumber $ mill) + 1000 * (htuToNumber $ thou) + (htuToNumber $ htu)
  where (mill,thou,htu) = mill_thou_htu ws

-- split the original string into millions, thousands and hundreds+tens+units
--
mill_thou_htu :: [String] -> ([String], [String], [String])
mill_thou_htu ws = (m, t, htu)
  where m    = if (find (=="million") ws) == Nothing then [] else takeWhile (/="million") ws
        ws'  = if (find (=="million") ws) == Nothing then ws else tail $ dropWhile (/="million") ws
        t    = if (find (=="thousand") ws') == Nothing then [] else takeWhile (/="thousand") ws'
        htu  = if (find (=="thousand") ws') == Nothing then ws' else tail $ dropWhile (/="thousand") ws'

-- split the htu part into hundreds, tens and units
-- the tens part will be a string ending with "ty"
--
h_t_u :: [String] -> ([String], [String], [String])
h_t_u ws = (h, t, u)
  where h   = if (find (=="hundred") ws) == Nothing then [] else takeWhile (/="hundred") ws
        ws' = if (find (=="hundred") ws) == Nothing then ws else tail $ dropWhile (/="hundred") ws
        t   = if (find (endsWith_ty) ws') == Nothing then [] else [head $ dropWhile (not . endsWith_ty) ws']
        u   = if (find (endsWith_ty) ws') == Nothing then ws' else tail $ dropWhile (not . endsWith_ty) ws'

-- test for word ending "ty" e.g. "twenty"
endsWith_ty :: String -> Bool
endsWith_ty ws = ((last ws) == 'y') && ((last $ init ws) == 't')

htuToNumber :: [String] -> Int
htuToNumber [] = 0
htuToNumber ws = 100 * (toNumber'' $ head' $ huns) + (toNumber'' $ head' $ tens) + (toNumber'' $ head' $ units)
  where (huns,tens,units) = h_t_u ws
        head' [] = []
        head' (x:xs) = x

toNumber'' :: String -> Int
toNumber'' s = if (Map.lookup s wordNumbers) /= Nothing then fromJust (Map.lookup s wordNumbers) else 0

-- we can use QuickCheck to test a bunch of numbers, converted to words and then back to numbers
prop_there_and_back n = (toNumber . toWords) (abs n) == (abs n)
