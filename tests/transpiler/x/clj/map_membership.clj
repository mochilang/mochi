(ns main)

(require 'clojure.set)

(defrecord M [a b])

(def m {"a" 1 "b" 2})

(defn -main []
  (println (contains? m "a"))
  (println (contains? m "c")))

(-main)
