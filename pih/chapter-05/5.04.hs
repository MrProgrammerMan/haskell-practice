replicate :: Int -> a -> [a]
replicate n val = [val | _ <- [1..n]]