main::IO ()

main = do
    print (bindExp 3) where
        bindExp x =
            let y = 11 in
                "the integer x was: " ++ show x
                ++ " and y was: " ++ show y