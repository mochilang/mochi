(ns main)

(defn _in [item col]
  (cond
    (and (string? col) (string? item)) (clojure.string/includes? col item)
    (map? col) (contains? col item)
    (sequential? col) (some #(= item %) col)
    :else false))
(declare xs ys m s)

(defn -main []
  (def xs [1 2 3]) ;; list of int
  (def ys (vec (->> (for [x xs :when (= (mod x 2) 1)] x)))) ;; list of int
  (println (some #(= 1 %) ys))
  (println (some #(= 2 %) ys))
  (def m {:a 1}) ;; 
  (println (_in "a" m))
  (println (_in "b" m))
  (def s "hello") ;; string
  (println (clojure.string/includes? s "ell"))
  (println (clojure.string/includes? s "foo"))
)

(-main)
