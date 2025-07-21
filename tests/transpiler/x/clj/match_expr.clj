(ns main)

(require 'clojure.set)

(def x 2)

(def label (cond (= x 1) "one" (= x 2) "two" (= x 3) "three" true "unknown"))

(defn -main []
  (println label))

(-main)
