(^*) :: Int -> Int -> Int
m ^* 0 = 1
m ^* n = m * (m ^* (n-1))