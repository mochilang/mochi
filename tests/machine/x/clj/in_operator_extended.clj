(ns main)

(declare xs ys m s)

(defn -main []
  (def xs [1 2 3]) ;; list of int
  (def ys (vec (->> (for [x xs :when (= (mod x 2) 1)] x)))) ;; list of int
  (println (some #(= 1 %) ys))
  (println (some #(= 2 %) ys))
  (def m {:a 1}) ;; map of string to int
  (println (contains? m "a"))
  (println (contains? m "b"))
  (def s "hello") ;; string
  (println (clojure.string/includes? s "ell"))
  (println (clojure.string/includes? s "foo"))
)

(-main)
