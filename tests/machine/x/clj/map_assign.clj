; Generated by Mochi compiler v0.10.28 on 2025-07-18T07:02:51Z
(ns main)

(defn _print [& args]
  (letfn [(pv [v]
            (cond
              (true? v) (print 1)
              (false? v) (print 0)
              (sequential? v) (doseq [[i x] (map-indexed vector v)]
                                (when (> i 0) (print " "))
                                (pv x))
              :else (print v)))]
    (doseq [[i a] (map-indexed vector args)]
      (when (> i 0) (print " "))
      (pv a))
    (println)))
(declare scores)

(defn -main []
  (def scores {"alice" 1}) ;; map of string to int
  (def scores (assoc scores "bob" 2)) ;; int
  (_print (get scores "bob"))
)

(-main)
