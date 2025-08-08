(ns main (:refer-clojure :exclude [fill_row min_path_sum]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare fill_row min_path_sum)

(def ^:dynamic fill_row_cell_n nil)

(def ^:dynamic fill_row_current_row nil)

(def ^:dynamic fill_row_left nil)

(def ^:dynamic fill_row_up nil)

(def ^:dynamic min_path_sum_cell_n nil)

(def ^:dynamic min_path_sum_current_row nil)

(def ^:dynamic min_path_sum_grid nil)

(def ^:dynamic min_path_sum_row_above nil)

(def ^:dynamic min_path_sum_row_n nil)

(defn fill_row [fill_row_current_row_p fill_row_row_above]
  (binding [fill_row_cell_n nil fill_row_current_row nil fill_row_left nil fill_row_up nil] (try (do (set! fill_row_current_row fill_row_current_row_p) (set! fill_row_current_row (assoc fill_row_current_row 0 (+ (nth fill_row_current_row 0) (nth fill_row_row_above 0)))) (set! fill_row_cell_n 1) (while (< fill_row_cell_n (count fill_row_current_row)) (do (set! fill_row_left (nth fill_row_current_row (- fill_row_cell_n 1))) (set! fill_row_up (nth fill_row_row_above fill_row_cell_n)) (if (< fill_row_left fill_row_up) (set! fill_row_current_row (assoc fill_row_current_row fill_row_cell_n (+ (nth fill_row_current_row fill_row_cell_n) fill_row_left))) (set! fill_row_current_row (assoc fill_row_current_row fill_row_cell_n (+ (nth fill_row_current_row fill_row_cell_n) fill_row_up)))) (set! fill_row_cell_n (+ fill_row_cell_n 1)))) (throw (ex-info "return" {:v fill_row_current_row}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn min_path_sum [min_path_sum_grid_p]
  (binding [min_path_sum_cell_n nil min_path_sum_current_row nil min_path_sum_grid nil min_path_sum_row_above nil min_path_sum_row_n nil] (try (do (set! min_path_sum_grid min_path_sum_grid_p) (when (or (= (count min_path_sum_grid) 0) (= (count (nth min_path_sum_grid 0)) 0)) (error "The grid does not contain the appropriate information")) (set! min_path_sum_cell_n 1) (while (< min_path_sum_cell_n (count (nth min_path_sum_grid 0))) (do (set! min_path_sum_grid (assoc-in min_path_sum_grid [0 min_path_sum_cell_n] (+ (nth (nth min_path_sum_grid 0) min_path_sum_cell_n) (nth (nth min_path_sum_grid 0) (- min_path_sum_cell_n 1))))) (set! min_path_sum_cell_n (+ min_path_sum_cell_n 1)))) (set! min_path_sum_row_above (nth min_path_sum_grid 0)) (set! min_path_sum_row_n 1) (while (< min_path_sum_row_n (count min_path_sum_grid)) (do (set! min_path_sum_current_row (nth min_path_sum_grid min_path_sum_row_n)) (set! min_path_sum_grid (assoc min_path_sum_grid min_path_sum_row_n (fill_row min_path_sum_current_row min_path_sum_row_above))) (set! min_path_sum_row_above (nth min_path_sum_grid min_path_sum_row_n)) (set! min_path_sum_row_n (+ min_path_sum_row_n 1)))) (throw (ex-info "return" {:v (nth (nth min_path_sum_grid (- (count min_path_sum_grid) 1)) (- (count (nth min_path_sum_grid 0)) 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_grid1 [[1 3 1] [1 5 1] [4 2 1]])

(def ^:dynamic main_grid2 [[1 0 5 6 7] [8 9 0 4 2] [4 4 4 5 1] [9 6 3 1 0] [8 4 3 2 7]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (min_path_sum main_grid1)))
      (println (str (min_path_sum main_grid2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
