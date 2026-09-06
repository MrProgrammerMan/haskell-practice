init [l] = []
init (l:ls) = l : Main.init ls
-- Old solution, produces warning: init ls = reverse (tail (reverse ls))

init2 [l] = []
init2 (l:ls) = l : init2 ls
-- Old solution, produces warning: init2 = reverse . tail . reverse

init3 [l] = []
init3 (l:ls) = l : init3 ls
-- Old solution, produces warnin: init3 = reverse . (drop 1) . reverse

initAlt [x] = []
initAlt (x:xs) = x : initAlt xs