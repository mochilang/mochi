(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main_nAssemblies main_partList)

(def main_partList ["A" "B" "C" "D"])

(def main_nAssemblies 3)

(defn -main []
  (doseq [cycle (range 1 (+ main_nAssemblies 1))] (do (println (str "begin assembly cycle " (str main_cycle))) (doseq [p main_partList] (println (str main_p " worker begins part"))) (doseq [p main_partList] (println (str main_p " worker completes part"))) (println (str (str "assemble.  cycle " (str main_cycle)) " complete")))))

(-main)
