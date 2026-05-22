(ns main)

(require 'clojure.set)

(defrecord M [a b c])

(def m {"a" 1 "b" 2 "c" 3})

(defn -main []
  (println (vals m)))

(-main)
