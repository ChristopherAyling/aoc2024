import Data.Char (isDigit)
import Data.List (sort)

extractDigitStrings :: (String, [String]) -> (String, [String])
extractDigitStrings ("", ds) = ("", filter (not . null) ds)
extractDigitStrings (str@(c:cs), ds) | isDigit c = extractDigitStrings (cs, (head ds++[c]):tail ds)
                        | not $ null (last ds) = extractDigitStrings (cs, []:ds)
                        | otherwise = extractDigitStrings (cs, ds)
                        
extractNumbers :: String -> [Int]
extractNumbers str = reverse $ map (\s -> read s :: Int) . snd $ extractDigitStrings (str, [[]])

pairNumbers :: [Int] -> [(Int, Int)]
pairNumbers [] = []
pairNumbers (n1:n2:ns) = (n1, n2):pairNumbers ns

main :: IO ()
main = do
    contents <- readFile "input_real.txt"
    let pairs = pairNumbers $ extractNumbers contents
    let leftSorted = sort $ map fst pairs
    let rightSorted = sort $ map snd pairs
    let diffs = map (\(l, r) -> abs(l-r)) $ zip leftSorted rightSorted
    print pairs
    print diffs
    print $ sum diffs