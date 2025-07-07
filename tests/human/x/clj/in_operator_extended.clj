(ns in-operator-extended
  (:require [clojure.string :as str]))

(def xs [1 2 3])
(def ys (filter odd? xs))
(println (boolean (some #{1} ys)))
(println (boolean (some #{2} ys)))

(def m {:a 1})
(println (contains? m :a))
(println (contains? m :b))

(def s "hello")
(println (str/includes? s "ell"))
(println (str/includes? s "foo"))
