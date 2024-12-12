levelsFromLine :: String -> [Int]
levelsFromLine line = map read (words line)

safeDifference :: Int -> Int -> Bool
safeDifference a b = let diff = abs (a-b) in 1 <= diff && diff <= 3

allSafeDifference :: [Int] -> Bool
allSafeDifference [] = True
allSafeDifference [x] = True
allSafeDifference (a:b:xs) = safeDifference a b && allSafeDifference (b:xs)

allInc :: [Int] -> Bool
allInc [] = True
allInc [x] = True
allInc (a:b:xs) = a <= b && allInc(b:xs)

allDec :: [Int] -> Bool
allDec [] = True
allDec [x] = True
allDec (a:b:xs) = a >= b && allDec(b:xs)

allIncOrDec :: [Int] -> Bool
allIncOrDec a = allInc a || allDec a

isSafe :: [Int] -> Bool
isSafe levels = allSafeDifference levels && (allInc levels || allDec levels)

main :: IO ()
main = do
    contents <- readFile "input_real.txt"
    let ls = lines contents
    let levels = map levelsFromLine ls
    -- print levels
    let  isSafeLevels = map isSafe levels
    let countSafeLevels = length $ filter id isSafeLevels
    print countSafeLevels
    -- print $ allDec [1, 2, 3]
    -- print $ allInc [1, 2, 3]