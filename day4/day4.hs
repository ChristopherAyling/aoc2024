import Debug.Trace (trace)
data Rule = Rule Int Int
    deriving (Show)

nomNumber :: String -> (Maybe Int, String)
nomNumber "" = (Nothing, "")
nomNumber xs = case reads xs :: [(Int, String)] of
    [(n, xs)] -> (Just n, xs)
    _ -> (Nothing, xs)

nomPipe :: String -> (Bool, String)
nomPipe "" = (False, "")
nomPipe ('|':xs) = (True, xs)
nomPipe (x:xs) = (False, xs)

nomComma :: String -> (Bool, String)
nomComma "" = (False, "")
nomComma (',':xs) = (True, xs)
nomComma (x:xs) = (False, xs)

nomRule :: String -> (Maybe Rule, String)
nomRule "" = (Nothing, "")
nomRule xs = case nomNumber xs of
    (Nothing, rest) -> (Nothing, rest)
    (Just nleft, rest) -> case nomPipe rest of
        (True, remaining) -> case nomNumber remaining of
            (Nothing, still) -> (Nothing, still)
            (Just nright, still) -> (Just $ Rule nleft nright, still)
        (False, still) -> (Nothing, xs)

nomRules :: String -> ([Rule], String)
nomRules "" = ([], "")
nomRules xs = case nomRule xs of
    (Nothing, rest) -> ([], rest)
    (Just rule, rest) ->
        let
            (rules, remaining) = nomRules rest
            in (rule:rules, remaining)

nomPageNumberLine :: String -> ([Int], String)
nomPageNumberLine "" = ([], "")
nomPageNumberLine xs = case nomNumber xs of
    (Nothing, rest) -> ([], rest)
    (Just n, rest) -> case nomComma rest of
        (True, rest) -> let (ns, remaining) = nomPageNumberLine rest in (n:ns, remaining)
        (False, rest) -> ([n], rest)


nomPageNumberLines :: String -> ([[Int]], String)
nomPageNumberLines "" = ([], "")
nomPageNumberLines xs = case nomPageNumberLine xs of
    ([], rest) -> ([], rest)
    (line, rest) ->
        let
            (morelines, remaining) = nomPageNumberLines rest
        in
            (line : morelines, remaining)

parseUpdates :: String -> ([Rule], [[Int]])
parseUpdates "" = ([], [])
parseUpdates xs =
    let (rules, rest) = nomRules xs
        (pages, _) = nomPageNumberLines rest
    in (rules, pages)

middle :: [Int] -> Int
middle xs =
    result
    where
        idx = length xs `div` 2
        result = xs !! idx

-- find the right number, then see if there are any left numbers after it
-- drop until right, if any left thereafter, fail. else pass.
-- if no right present, pass
obeysRule :: Rule -> [Int] -> Bool
obeysRule (Rule left right) update = case dropWhile (/=right) update of
    [] -> True
    rest -> left `notElem` rest


-- recurse down the list of rules, return when out of rules or a rule is not met
obeysRules :: [Rule] -> [Int] -> Bool
obeysRules [] update = True
obeysRules (rule:rest) update = obeysRule rule update && obeysRules rest update

main :: IO()
main = do
    contents <- readFile "input_real.txt"
    let (rules, updates) = parseUpdates contents
    let check = obeysRules rules
    let obeyers = filter check updates
    print obeyers
    let middles = map middle obeyers
    print middles
    print $ sum middles