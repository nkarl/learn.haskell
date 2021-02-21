
aa = [1,2,3,4,5,6,7,8,9,10]

--
-- original copylist
copylist [] = []
copylist (x:xs) = x:(copylist xs)

-- new
-- [1,2,3] --> [[1],[1,2],[1,2,3]]
cpl2 [] buf = (reverse buf):[]
cpl2 (x:xs) buf = (reverse buf):(cpl2 xs (x:buf))


-- better
copyl2 [] buf = (reverse buf):[]
copyl2 (x:xs) buf | (length buf) >= 3 = (reverse buf):(copyl2 xs (x:[]))
                    | otherwise = (copyl2 xs (x:buf))


group3 iL = grouphelper iL []
grouphelper [] buf = (reverse buf):[]
grouphelper (x:xs) buf | (length buf) >= 3 = (reverse buf):(grouphelper xs (x:[]))
                    | otherwise = (grouphelper xs (x:buf))

