(ns main (:refer-clojure :exclude [main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(defn main []
  (do (def a (int (read-line))) (def b (int (read-line))) (println (+ a b))))

(defn -main []
  (main))

(-main)
