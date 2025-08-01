(ns main (:refer-clojure :exclude [main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main)

(declare main_a main_d main_e main_i main_list)

(defn main []
  (do (def main_list []) (def main_a 1) (def main_d 2) (def main_e 3) (def main_i 4) (def main_list (conj main_list main_a)) (def main_list (conj main_list main_d)) (def main_list (conj main_list main_e)) (def main_list (conj main_list main_i)) (def main_i (count main_list))))

(defn -main []
  (main))

(-main)
