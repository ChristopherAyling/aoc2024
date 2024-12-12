nomXmul :: String -> String
nomXmul "" = ""
nomXmul ('m':'u':'l':'(':xs) = xs
nomXmul (x:xs) = nomXmul xs

nomNumber :: String -> (Maybe Int, String)
nomNumber "" = (Nothing, "")
nomNumber xs = case reads xs :: [(Int, String)] of
    [(n, xs)] -> (Just n, xs)
    _ -> (Nothing, xs)

nomComma :: String -> (Bool, String)
nomComma "" = (False, "")
nomComma (',':xs) = (True, xs)
nomComma (x:xs) = (False, xs)

nomRightParen :: String -> (Bool, String)
nomRightParen "" = (False, "")
nomRightParen (')':xs) = (True, xs)
nomRightParen (x:xs) = (False, xs)

-- parsing this string
-- xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))

nomMulExpr :: String -> (Int, String)
nomMulExpr "" = (0, "")
nomMulExpr xs = case nomXmul xs of 
    "" -> (0, "")
    xs -> case nomNumber xs of
        (Nothing, xs) -> (0, xs)
        (Just nleft, xs) -> case nomComma xs of
            (True, xs) -> case nomNumber xs of
                (Nothing, xs) -> (0, xs)
                (Just nright, xs) -> case nomRightParen xs of
                    (True, xs) -> (nleft * nright, xs)
                    (False, xs) -> (0, xs)
            (False, xs) -> (0, xs)

nomAllMulExprs :: String -> Int
nomAllMulExprs "" = 0
nomAllMulExprs xs = let (n, cs) = nomMulExpr xs in n + nomAllMulExprs cs


main :: IO()
main = do
    contents <- readFile "input_real.txt"
    -- let (n1, cs) = nomMulExpr contents
    -- print  (n1, cs)
    -- let (n2, cs2) = nomMulExpr cs
    -- print (n2, cs2)
    print $ nomAllMulExprs contents
