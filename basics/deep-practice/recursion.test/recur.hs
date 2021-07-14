
fact :: (Integral a) => a -> a
fact 1 = 1
fact n = n * fact (n-1)


factTail :: (Integral a) => a -> a
factTail n = helper n 1 where
    helper 1 memo = memo
    helper n memo = helper (n-1) (n*memo)


factTailStrict :: (Integral a) => a -> a
factTailStrict n = helper n 1 where
    helper 1 memo = memo
    helper n memo = helper (n-1) $! (n*memo)
