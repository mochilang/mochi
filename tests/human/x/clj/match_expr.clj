(ns match-expr)

(def x 2)

(def label
  (case x
    1 "one"
    2 "two"
    3 "three"
    "unknown"))

(println label)
