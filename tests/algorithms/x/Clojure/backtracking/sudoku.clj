(ns main (:refer-clojure :exclude [is_safe find_empty_location sudoku print_solution]))

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

(declare is_safe find_empty_location sudoku print_solution)

(def ^:dynamic main_idx nil)

(def ^:dynamic print_solution_line nil)

(def ^:dynamic sudoku_column nil)

(def ^:dynamic sudoku_grid nil)

(def ^:dynamic sudoku_loc nil)

(def ^:dynamic sudoku_row nil)

(defn is_safe [is_safe_grid is_safe_row is_safe_column is_safe_n]
  (try (do (dotimes [i 9] (when (or (= (nth (nth is_safe_grid is_safe_row) i) is_safe_n) (= (nth (nth is_safe_grid i) is_safe_column) is_safe_n)) (throw (ex-info "return" {:v false})))) (dotimes [i 3] (dotimes [j 3] (when (= (nth (nth is_safe_grid (+ (- is_safe_row (mod is_safe_row 3)) i)) (+ (- is_safe_column (mod is_safe_column 3)) j)) is_safe_n) (throw (ex-info "return" {:v false}))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn find_empty_location [find_empty_location_grid]
  (try (do (dotimes [i 9] (dotimes [j 9] (when (= (nth (nth find_empty_location_grid i) j) 0) (throw (ex-info "return" {:v [i j]}))))) (throw (ex-info "return" {:v []}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sudoku [sudoku_grid_p]
  (binding [sudoku_column nil sudoku_grid nil sudoku_loc nil sudoku_row nil] (try (do (set! sudoku_grid sudoku_grid_p) (set! sudoku_loc (find_empty_location sudoku_grid)) (when (= (count sudoku_loc) 0) (throw (ex-info "return" {:v true}))) (set! sudoku_row (nth sudoku_loc 0)) (set! sudoku_column (nth sudoku_loc 1)) (doseq [digit (range 1 10)] (when (is_safe sudoku_grid sudoku_row sudoku_column digit) (do (set! sudoku_grid (assoc-in sudoku_grid [sudoku_row sudoku_column] digit)) (when (sudoku sudoku_grid) (throw (ex-info "return" {:v true}))) (set! sudoku_grid (assoc-in sudoku_grid [sudoku_row sudoku_column] 0))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_solution [print_solution_grid]
  (binding [print_solution_line nil] (dotimes [r (count print_solution_grid)] (do (set! print_solution_line "") (dotimes [c (count (nth print_solution_grid r))] (do (set! print_solution_line (str print_solution_line (str (nth (nth print_solution_grid r) c)))) (when (< c (- (count (nth print_solution_grid r)) 1)) (set! print_solution_line (str print_solution_line " "))))) (println print_solution_line)))))

(def ^:dynamic main_initial_grid [[3 0 6 5 0 8 4 0 0] [5 2 0 0 0 0 0 0 0] [0 8 7 0 0 0 0 3 1] [0 0 3 0 1 0 0 8 0] [9 0 0 8 6 3 0 0 5] [0 5 0 0 9 0 6 0 0] [1 3 0 0 0 0 2 5 0] [0 0 0 0 0 0 0 7 4] [0 0 5 2 0 6 3 0 0]])

(def ^:dynamic main_no_solution [[5 0 6 5 0 8 4 0 3] [5 2 0 0 0 0 0 0 2] [1 8 7 0 0 0 0 3 1] [0 0 3 0 1 0 0 8 0] [9 0 0 8 6 3 0 0 5] [0 5 0 0 9 0 6 0 0] [1 3 0 0 0 0 2 5 0] [0 0 0 0 0 0 0 7 4] [0 0 5 2 0 6 3 0 0]])

(def ^:dynamic main_examples [main_initial_grid main_no_solution])

(def ^:dynamic main_idx 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_idx (count main_examples)) (do (println "\nExample grid:\n====================") (print_solution (nth main_examples main_idx)) (println "\nExample grid solution:") (if (sudoku (nth main_examples main_idx)) (print_solution (nth main_examples main_idx)) (println "Cannot find a solution.")) (def main_idx (+ main_idx 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
