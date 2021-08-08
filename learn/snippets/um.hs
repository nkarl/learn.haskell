usa = 315000000.0
mex = 105000000.0

compute u m
    | m > u = (u, m)
    | otherwise = compute (u - u * 0.15) (m + m * 1.15)
