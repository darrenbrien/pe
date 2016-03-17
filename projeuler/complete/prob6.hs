sumSquares = sum $ map (^2) [1..100]

squareSum = (^2) $ sum [1..100]

res = squareSum - sumSquares
	
