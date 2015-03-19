cutSame :: (Eq x) => [x] -> [x]
cutSame [] = []
cutSame [x] = [x]
cutSame (a:b:xs) = if (a == b)
	then (a:(cutSame (b:xs)))
	else [a]

cutPair :: (Eq x) => [x] -> ([x], [x])
cutPair [] = ([], [])
cutPair [x] = ([x], [])
cutPair xs = (same, drop (length same) xs )
		where same = cutSame xs

pack :: (Eq x) => [x] -> [[x]]
pack [] = []
pack [x] = [[x]]
pack xs = let (beginning,rest) = cutPair xs in (beginning:(pack rest))
