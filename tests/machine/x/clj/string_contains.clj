(ns main)

(declare s)

(defn -main []
  (def s "catch") ;; string
  (println (clojure.string/includes? s "cat"))
  (println (clojure.string/includes? s "dog"))
)

(-main)
