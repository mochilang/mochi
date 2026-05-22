(ns main)

(require 'clojure.set)

(defrecord Data [outer])

(def data {"outer" {"inner" 1}})

(defn -main []
  (def data (assoc-in data ["outer" "inner"] 2))
  (println (get (get data "outer") "inner")))

(-main)
