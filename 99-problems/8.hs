compress :: (Eq x) => [x] -> [x]
compress [] = []
compress [x] = [x]
compress (a:b:xs) 
	| a == b = compress (b:xs)
	| otherwise = a:(compress (b:xs))
