import Data.List

generator2 :: [(String,String,String,String,String)]
generator2 =
  [(n1, n2, n3, n4, n5)
  | n1 <- filter (\s -> nub s == s && '0' `notElem` s) (map show [102..987]), 
    n2 <- filter (\x -> head x /= head n1) (comb 2 n1), 
    n3 <- permutations n1,
    n4 <- comb 2 n1,
    n5 <- permutations n1
    ]

uniqueDigits :: String -> Bool
uniqueDigits s = nub s == s

comb :: Eq a => Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb n xs = [i:s | i <- xs, s <- comb (n-1) (xs \\ [i])]

tester2 :: (String,String,String,String,String) -> Bool
tester2 (a,b,c,d,e) = 
  n1 - n2 == n3
  && n3 - n4 == n5
  && n1 + n3 + n5 < 2000
  where
  [n1,n2,n3,n4,n5] = map read [a,b,c,d,e]

x_generator2 :: Int
x_generator2 =
  length [t | t <- ts , t `elem ` g]
  where
  g = generator2
  ts =
   [ ("123","21","123","12","123")
   , ("162","26","261","12","621")
   , ("219","19","912","21","291")
   , ("329","92","932","32","239")
   , ("439","94","394","43","394")
   , ("549","95","945","95","945")
   , ("568","68","586","56","586")
   , ("769","67","679","97","796")
   , ("879","79","897","98","789")
   , ("987","79","789","79","789")
   ]

x_tester2 :: Int
x_tester2 =
  length [t | t <- ts , tester2 t]
  where
  ts =
      [ ("138","01","137","50","87")
    , (" 143","01","142 ","52","90")
    , (" 171","02","169 ","79","90")
    , (" 152","03","149 ","54","95")
    , (" 159","04","155 ","61","94")
    , (" 161","05","156 ","63","93")
    , (" 182","06","176 ","80","96")
    , (" 151","07","144 ","57","87")
    , (" 165","08","157 ","64","93")
    , (" 174","09","165 ","71","94")
    ]

main :: IO()
main
  = print(filter tester2 generator2)
