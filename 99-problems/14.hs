dupli :: [x] -> [x]
dupli [] = []
dupli (x:xs) = (x:x:dupli xs)
