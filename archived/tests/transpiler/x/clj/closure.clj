(ns main (:refer-clojure :exclude [makeAdder]))

(require 'clojure.set)

(defn makeAdder [n]
  (fn [x] (+ x n)))

(def add10 (makeAdder 10))

(defn -main []
  (println (add10 7)))

(-main)
