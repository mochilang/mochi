(ns if-then-else-nested)

(def x 8)
(def msg (if (> x 10) "big" (if (> x 5) "medium" "small")))
(println msg)
