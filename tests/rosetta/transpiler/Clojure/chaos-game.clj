(ns main (:refer-clojure :exclude [randInt]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare randInt)

(declare main_grid main_height main_i main_idx main_iterations main_line main_px main_py main_r main_seed main_v main_vertices main_width main_x main_y next_v)

(def main_width 60)

(def main_height (int (* (double main_width) 0.86602540378)))

(def main_iterations 5000)

(def main_grid [])

(def main_y 0)

(defn randInt [randInt_s randInt_n]
  (try (do (def next_v (mod (+ (* randInt_s 1664525) 1013904223) 2147483647)) (throw (ex-info "return" {:v [next_v (mod next_v randInt_n)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_seed 1)

(def main_vertices [[0 (- main_height 1)] [(- main_width 1) (- main_height 1)] [(int (/ main_width 2)) 0]])

(def main_px (int (/ main_width 2)))

(def main_py (int (/ main_height 2)))

(def main_i 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_y main_height) (do (def main_line []) (def main_x 0) (while (< main_x main_width) (do (def main_line (conj main_line " ")) (def main_x (+ main_x 1)))) (def main_grid (conj main_grid main_line)) (def main_y (+ main_y 1))))
      (while (< main_i main_iterations) (do (def main_r (randInt main_seed 3)) (def main_seed (nth main_r 0)) (def main_idx (int (nth main_r 1))) (def main_v (nth main_vertices main_idx)) (def main_px (int (/ (+ main_px (nth main_v 0)) 2))) (def main_py (int (/ (+ main_py (nth main_v 1)) 2))) (when (and (and (and (>= main_px 0) (< main_px main_width)) (>= main_py 0)) (< main_py main_height)) (def main_grid (assoc-in main_grid [main_py main_px] "*"))) (def main_i (+ main_i 1))))
      (def main_y 0)
      (while (< main_y main_height) (do (def main_line "") (def main_x 0) (while (< main_x main_width) (do (def main_line (+ main_line (nth (nth main_grid main_y) main_x))) (def main_x (+ main_x 1)))) (println main_line) (def main_y (+ main_y 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
