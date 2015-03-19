pack :: (Eq x) => [x] -> [[x]]
pack [] = [[]]
pack [x] = [[x]]
pack xs
	where cutSame xs =
		case xs of 
			[] = ([], [])
			[x] = ([x], [])
			(a:b:xs) = if a == b then (a:slice, rest) else (
				where (slice, rest) = cutSame(b:xs)
