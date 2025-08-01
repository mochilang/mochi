(ns main (:refer-clojure :exclude [randInt]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare randInt)

(declare i idx line main_grid main_height main_iterations main_line main_width main_x main_y next_v px py r seed v vertices x)

(def main_width 60)

(def main_height (int (* (double main_width) 0.86602540378)))

(def main_iterations 5000)

(def main_grid [])

(def main_y 0)

(defn randInt [randInt_s randInt_n]
  (try (do (def next_v (mod (+ (* randInt_s 1664525) 1013904223) 2147483647)) (throw (ex-info "return" {:v [next_v (mod next_v randInt_n)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def seed 1)

(def vertices [[0 (- height 1)] [(- width 1) (- height 1)] [(int (/ width 2)) 0]])

(def px (int (/ width 2)))

(def py (int (/ height 2)))

(def i 0)

(defn -main []
  (while (< main_y main_height) (do (def main_line []) (def main_x 0) (while (< main_x main_width) (do (def main_line (conj main_line " ")) (def main_x (+ main_x 1)))) (def main_grid (conj main_grid main_line)) (def main_y (+ main_y 1))))
  (while (< i iterations) (do (def r (randInt seed 3)) (def seed (nth r 0)) (def idx (int (nth r 1))) (def v (nth vertices idx)) (def px (int (/ (+ px (nth v 0)) 2))) (def py (int (/ (+ py (nth v 1)) 2))) (when (and (and (and (>= px 0) (< px width)) (>= py 0)) (< py height)) (def grid (assoc-in grid [py px] "*"))) (def i (+ i 1))))
  (def y 0)
  (while (< y height) (do (def line "") (def x 0) (while (< x width) (do (def line (str line (nth (nth grid y) x))) (def x (+ x 1)))) (println line) (def y (+ y 1)))))

(-main)
