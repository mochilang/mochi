; Generated by Mochi compiler v0.10.28 on 2025-07-18T07:03:08Z
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
(defn _sort_key [k]
  (cond
    (map? k) (pr-str (into (sorted-map) k))
    (sequential? k) (vec k)
    :else k))
(declare data sorted)

(defn -main []
  (def data [{:a 1 :b 2} {:a 1 :b 1} {:a 0 :b 5}]) ;; list of
  (def sorted (vec (->> (for [x data] x) (sort-by (fn [x] (_sort_key {:a (:a x) :b (:b x)})))))) ;; list of
  (_print sorted)
)

(-main)
