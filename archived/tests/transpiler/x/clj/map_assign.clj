(ns main)

(require 'clojure.set)

(defrecord Scores [alice])

(def scores {"alice" 1})

(defn -main []
  (def scores (assoc scores "bob" 2))
  (println (get scores "bob")))

(-main)
