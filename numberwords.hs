import Data.List

-- deal with special cases here
--
toWords  :: Int -> String
toWords 1000000000 = "one billion"
toWords 0 = "zero"
toWords n = concat $ intersperse " " $ words $ toWords' n      -- clean up multiple or trailing spaces

-- workhorse function
toWords' :: Int -> String
toWords' n
  | n>=1000000000 = "too big"
  | n>=1000000 = (toWords' $ n `div` 1000000) ++ " million " ++ (toWords' $ n `mod` 1000000)
  | n>=1000 = (toWords' $ n `mod` 1000000 `div` 1000) ++ " thousand " ++ (toWords' $ n `mod` 1000)
  | n>=100 = (toWords' $ n `div` 100) ++ " hundred " ++ (toWords' $ n `mod` 100)
  | n>=20 = let units = (n `mod` 10) in (toTens (n - units)) ++ " " ++ (toWords' units)
  | n==19 = "nineteen"
  | n==18 = "eighteen"
  | n==17 = "seventeen"
  | n==16 = "sixteen"
  | n==15 = "fifteen"
  | n==14 = "fourteen"
  | n==13 = "thirteen"
  | n==12 = "twelve"
  | n==11 = "eleven"
  | n==10 = "ten"
  | n==9 = "nine"
  | n==8 = "eight"
  | n==7 = "seven"
  | n==6 = "six"
  | n==5 = "five"
  | n==4 = "four"
  | n==3 = "three"
  | n==2 = "two"
  | n==1 = "one"
  | otherwise = ""

toTens :: Int -> String
toTens n
  | n==90 = "ninety"
  | n==80 = "eighty"
  | n==70 = "seventy"
  | n==60 = "sixty"
  | n==50 = "fifty"
  | n==40 = "forty"
  | n==30 = "thirty"
  | n==20 = "twenty"
