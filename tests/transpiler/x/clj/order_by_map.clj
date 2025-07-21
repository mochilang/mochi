(ns main)

(require 'clojure.set)

(defrecord Data [a b])

(def data [{:a 1 :b 2} {:a 1 :b 1} {:a 0 :b 5}])

(def sorted (for [x (sort-by (fn [x] [(:a x) (:b x)]) data)] x))

(defn -main []
  (println sorted))

(-main)
