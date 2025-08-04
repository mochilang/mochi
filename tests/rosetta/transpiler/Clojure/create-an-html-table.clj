(ns main (:refer-clojure :exclude [main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main)

(def ^:dynamic main_idx nil)

(def ^:dynamic main_rows nil)

(defn main []
  (binding [main_idx nil main_rows nil] (do (set! main_rows []) (dotimes [i 4] (set! main_rows (conj main_rows [(* i 3) (+ (* i 3) 1) (+ (* i 3) 2)]))) (println "<table>") (println "    <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>") (set! main_idx 0) (doseq [row main_rows] (do (println (str (str (str (str (str (str (str (str "    <tr><td>" (str main_idx)) "</td><td>") (str (nth row 0))) "</td><td>") (str (nth row 1))) "</td><td>") (str (nth row 2))) "</td></tr>")) (set! main_idx (+ main_idx 1)))) (println "</table>"))))

(defn -main []
  (main))

(-main)
