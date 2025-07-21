(ns main)

(require 'clojure.set)

(defrecord M [a])

(def xs [1 2 3])

(def ys (for [x xs :when (= (mod x 2) 1)] x))

(def m {:a 1})

(def s "hello")

(defn -main []
  (println (boolean (some #{1} ys)))
  (println (boolean (some #{2} ys)))
  (println (contains? m "a"))
  (println (contains? m "b"))
  (println (clojure.string/includes? s "ell"))
  (println (clojure.string/includes? s "foo")))

(-main)
