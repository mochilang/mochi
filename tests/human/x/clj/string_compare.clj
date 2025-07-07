(ns string-compare)

(println (neg? (compare "a" "b")))
(println (<= (compare "a" "a") 0))
(println (pos? (compare "b" "a")))
(println (>= (compare "b" "b") 0))
