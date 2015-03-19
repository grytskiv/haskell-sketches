repli :: [x] -> Int -> [x]
repli [] n = []
repli (x:xs) n = ((replicate n x)++(repli xs n))
