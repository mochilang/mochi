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

(def ^:dynamic main_a nil)

(def ^:dynamic main_col nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_row nil)

(def ^:dynamic main_rowArr nil)

(defn main []
  (binding [main_a nil main_col nil main_i nil main_j nil main_row nil main_rowArr nil] (do (set! main_row 3) (set! main_col 4) (set! main_a []) (set! main_i 0) (while (< main_i main_row) (do (set! main_rowArr []) (set! main_j 0) (while (< main_j main_col) (do (set! main_rowArr (conj main_rowArr 0)) (set! main_j (+ main_j 1)))) (set! main_a (conj main_a main_rowArr)) (set! main_i (+ main_i 1)))) (println (str "a[0][0] = " (str (nth (nth main_a 0) 0)))) (set! main_a (assoc-in main_a [(long (- main_row 1)) (long (- main_col 1))] 7)) (println (str (str (str (str (str "a[" (str (- main_row 1))) "][") (str (- main_col 1))) "] = ") (str (nth (nth main_a (long (- main_row 1))) (long (- main_col 1)))))) (set! main_a nil))))

(defn -main []
  (main))

(-main)
