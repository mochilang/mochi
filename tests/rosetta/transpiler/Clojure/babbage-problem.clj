(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(defn -main []
  (def target 269696)
  (def modulus 1000000)
  (def n 1)
  (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def square (* n n)) (def ending (mod square modulus)) (cond (= ending target) (do (println (str (str (str "The smallest number whose square ends with " (str target)) " is ") (str n))) (recur false)) :else (do (def n (+ n 1)) (recur while_flag_1)))))))

(-main)
