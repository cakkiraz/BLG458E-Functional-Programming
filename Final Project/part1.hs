dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d =
    (d + t1 + k + t2 + t3 + 5 * j) `mod` 7
    where
        -- If month is equal or less than 2 it returns (y-1,m+12)
        dayOfWeek' :: Integer -> Integer -> [Integer]
        dayOfWeek' y m
            | m <= 2 = [y-1,m+12]
            | otherwise = [y,m]
        [y',m'] = dayOfWeek' y m
        j = y' `div` 100
        k = y' `mod` 100
        t1 = floor (fromIntegral (13 * (m' + 1)) / 5.0)
        t2 = floor (fromIntegral (k) / 4.0)
        t3 = floor (fromIntegral (j) / 4.0)

{- This function counts how many months's first day is sunday between two years.
What does the helper function (sundays') calculate? : Helper function takes year and month. If first day of month is sunday it returns functions itself with increasing rest by 1. Until y greater than end year, it iterates recursively. Year and month are updated using mod operation.
What if you don't define a "rest" and use its expression where it's needed? : When we are using recursive approach we need rest. If we dont use rest, we can use tail recursive approach. We can give number of years as parameter to the function. At the end of the iterations we return directly that parameter.
-}
sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1
    where
        sundays' :: Integer -> Integer -> Integer
        sundays' y m
            | y > end   = 0
            -- If it is sunday increase rest by 1
            | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
            where
                nextM = (m `mod` 12) + 1
                nextY = if m == 12 then y+1 else y
                rest = sundays' nextY nextM

{- This function tail recursive version of above function.
It takes numberofsundays as a parameter. Every iteration if conditions are valid, it increases by 1.
At the final iteration the function returns that parameter(rest).
-}
sundays1tr :: Integer -> Integer -> Integer
sundays1tr start end = tailRecursiveSundays' start 1 0
    where
        tailRecursiveSundays' :: Integer -> Integer-> Integer -> Integer
        tailRecursiveSundays' y m rest
            | y > end   = rest
            | otherwise = if dayOfWeek y m 1 == 1 then tailRecursiveSundays' nextY nextM (rest+1) else tailRecursiveSundays' nextY nextM rest
            where
                nextM = (m `mod` 12) + 1
                nextY = if m == 12 then y+1 else y

{- This function takes an integer and returns whether given conditions are true -}
leap :: Integer -> Bool
leap y = ((y `mod` 4) == 0 && (y`mod` 100) /= 0 || (y `mod` 400) == 0)

{- This function takes month and year than returns there are how many days in that month.-}
days_in_month :: Integer -> Integer -> Integer
days_in_month m y
    | m == 2 =
        case leap(y) of
            True -> 29
            False -> 28
    | (m == 4 || m == 6 || m == 9 || m == 11) = 30
    | otherwise = 31

{- This function makes same thing with sunday1 function. I have used tail recursive approach for this function.-}
sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays2' start 1 2 0
    where
        sundays2' :: Integer -> Integer -> Integer -> Integer -> Integer
        sundays2' y m weekday n
            | y > end = n
            | (new_weekday `mod` 7) == 0 = sundays2' new_year new_month new_weekday (n+1)
            | otherwise = sundays2' new_year new_month new_weekday (n)
            where
                days = days_in_month m y
                new_month = ((m) `mod` 12) + 1
                new_year = if m == 12 then y+1 else y
                new_weekday = weekday + (days `mod` 7)

getFunction :: String -> (Integer -> Integer -> Integer)
getFunction name
    | name == "sundays1"   = sundays1
    | name == "sundays1tr" = sundays1tr
    | name == "sundays2"   = sundays2
    | otherwise            = error "unknown function"

main :: IO ()
main = do
line <- getLine
let [f, start, end] = words line
putStrLn $ show $ (getFunction f) (read start :: Integer) (read end :: Integer)

-- def sundays2(start, end):
--     n = 0
--     weekday = 2
--     for y in range(start, end + 1):
--         for m in range(1, 13):
--             days = days_in_month(m, y)
--             weekday += days % 7
--             if weekday % 7 == 0:
--                 n += 1
--     return n

-- calculateNumberOfDays :: Integer -> Integer
-- calculateNumberOfDays year = calculateNumberOfDays year + 1
--     where
--         calculateNumberOfDays' :: Integer -> Integer -> Integer
--         calculateNumberOfDays' m y

