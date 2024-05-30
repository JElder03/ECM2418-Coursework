import qualified Data.Set as Set

-- tests if a generated data/time tuple is magic, and returns true if it is, false otherwise
-- A tuple is magic if the displayed date/time digits are all different, and the number of lit
-- segments when displaying the date/time on a 7-segment display is prime
magic :: [Int] -> Bool
magic datetime = 
  Set.size (Set.fromList datetime) == 8 && prime (totalNumSeg datetime)

datetimeToList :: (Int, Int, Int, Int) -> [Int]
datetimeToList dt =
   remove2Tuples (map digits (toList dt))

-- Splits an integer into its component digits
-- Works for integers <100 only
digits :: Integral x => x -> (x,x)
digits x = 
  (x `div` 10, x `mod` 10)

-- returns the number of segments any integer lights up
numSeg :: Int -> Int
numSeg n
  | n `elem` [0,6,9] = 6
  | n == 1 = 2
  | n == 4 = 4
  | n == 7 = 3
  | n == 8 = 7
  | otherwise = 5

-- returns the last day of each month
lastDayOFMonth :: Int -> Int
lastDayOFMonth mt
  | mt == 2 = 28
  | mt `elem` [1, 3, 5, 7, 8, 10, 12] = 31
  | otherwise = 30

-- Adds one day to the date/time tuple
addDay :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addDay (hr,mn,dy,mt)
  | dy < lastDayOFMonth mt = (hr,mn,dy+1,mt) -- safe to add day
  | mt == 12 = (hr,mn,01,01) -- last day of year
  | otherwise = (hr,mn,01,mt+1) -- last day of month

-- Adds one day to the date/time tuple
addMin :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addMin (hr, mn, dy, mt) 
  | mn < 59 = (hr,mn+1,dy,mt) --safe to add min
  | hr == 23 && dy == lastDayOFMonth mt && mt == 12 = (00,00,01,01) -- last minute of year
  | hr == 23 && dy == lastDayOFMonth mt = (00,00,01,mt + 1) -- last minute of month
  | hr == 23 = (00,00,dy + 1,mt) -- last minute of day
  | otherwise = (hr + 1,00,dy,mt) -- last minute of hour

-- Finds the total number of segments lit up by a datetime
totalNumSeg :: [Int] -> Int
totalNumSeg dtDigits =
  sum (map numSeg dtDigits)

toList :: (a,a,a,a) -> [a]
toList (a,b,c,d)
   = [a,b,c,d]

remove2Tuples :: [(a,a)] -> [a]
remove2Tuples [] =
  []
remove2Tuples ((a,b):xs) =
  [a,b] ++ remove2Tuples xs

-- tests if a number is prime, and returns true if it is, false otherwise
prime :: Int -> Bool
prime
   = not . factorisable 2

-- tests if factorisable from at least one number >= some integer
factorisable :: Int -> Int -> Bool
factorisable f n
   | f * f <= n = n `mod` f == 0 || factorisable (f+1) n
   | otherwise = False

--TESTER GENERATOR--
-- generates all possuble solutions to Teaser 1
generator1 :: [(Int,Int,Int,Int)]
generator1 = 
  [(hr,mn,dy,mt)
  | hr <- [0..23],
    mn <- [0..59],
    mt <- [1..12],
    dy <- [1..lastDayOFMonth mt]
    ]

-- tests all generated data/time tuples, and returns true if the tuple is a solution to Teaser 1, false otherwise
tester1 :: (Int, Int, Int, Int) -> Bool
tester1 x = 
  magic datetime 
  && magic nextDay 
  && (totalNumSeg datetime + totalNumSeg nextDay) `div` 2 == totalNumSeg nextMin
  where
  datetime = datetimeToList x
  nextDay = datetimeToList (addDay x)
  nextMin = datetimeToList (addMin (addDay x))

--TESTERS--
x_generator1 :: Int
x_generator1 =
  length [t | t <- ts , t `elem ` g]
  where
  g = generator1
  ts =
    [ ( 2 ,15 ,14 ,11)
    , ( 4 ,31 ,27 , 9)
    , ( 6 ,47 ,10 , 8)
    , ( 9, 3,23, 6)
    , (11 ,19 , 6, 5)
    , (13 ,35 ,19 , 3)
    , (15 ,51 , 2, 2)
    , (18 , 6 ,16 ,12)
    , (20 ,22 ,29 ,10)
    , (22 ,38 ,11 , 9)
    ]

x_tester1 :: Int
x_tester1 =
  length [t | t <- ts , tester1 t]
  where
  ts =
    [ ( 6 ,59 ,17 ,24)
    , ( 6 ,59 ,17 ,34)
    , ( 6 ,59 ,27 ,14)
    , ( 6 ,59 ,27 ,41)
    , ( 8 ,59 ,12 ,46)
    , (16 ,59 , 7 ,24)
    , (16 ,59 , 7 ,42)
    , (16 ,59 , 7 ,43)
    , (16 ,59 ,27 ,40)
    , (18 ,59 , 2 ,46)
    ]

main :: IO()
main
  = print(filter tester1 generator1)
