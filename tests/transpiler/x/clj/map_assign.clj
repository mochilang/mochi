(ns main)

(require 'clojure.set)

(defrecord Anon1 [alice])

(def scores {"alice" 1})

(defn -main []
  (def scores (assoc scores "bob" 2))
  (println (get scores "bob")))

(-main)
