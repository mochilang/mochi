(ns main (:refer-clojure :exclude [bar main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare bar main)

(declare main_args)

(defn bar [bar_a bar_b bar_c]
  (println (str (str (str (str (str bar_a) ", ") (str bar_b)) ", ") (str bar_c))))

(defn main []
  (do (def main_args {}) (def main_args (assoc main_args "a" 3)) (def main_args (assoc main_args "b" 2)) (def main_args (assoc main_args "c" 1)) (bar (get main_args "a") (get main_args "b") (get main_args "c"))))

(defn -main []
  (main))

(-main)
