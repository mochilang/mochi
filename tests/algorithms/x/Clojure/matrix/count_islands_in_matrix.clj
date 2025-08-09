(ns main (:refer-clojure :exclude [is_safe dfs count_islands]))

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

(declare is_safe dfs count_islands)

(def ^:dynamic count_islands_cols nil)

(def ^:dynamic count_islands_i nil)

(def ^:dynamic count_islands_j nil)

(def ^:dynamic count_islands_row_list nil)

(def ^:dynamic count_islands_rows nil)

(def ^:dynamic count_islands_visited nil)

(def ^:dynamic count_v nil)

(def ^:dynamic dfs_col_nbr nil)

(def ^:dynamic dfs_k nil)

(def ^:dynamic dfs_new_col nil)

(def ^:dynamic dfs_new_row nil)

(def ^:dynamic dfs_row_nbr nil)

(def ^:dynamic dfs_visited nil)

(def ^:dynamic is_safe_cols nil)

(def ^:dynamic is_safe_not_visited nil)

(def ^:dynamic is_safe_rows nil)

(def ^:dynamic is_safe_visited_cell nil)

(def ^:dynamic is_safe_within_bounds nil)

(defn is_safe [is_safe_grid is_safe_visited is_safe_row is_safe_col]
  (binding [is_safe_cols nil is_safe_not_visited nil is_safe_rows nil is_safe_visited_cell nil is_safe_within_bounds nil] (try (do (set! is_safe_rows (count is_safe_grid)) (set! is_safe_cols (count (nth is_safe_grid 0))) (set! is_safe_visited_cell (nth (nth is_safe_visited is_safe_row) is_safe_col)) (set! is_safe_within_bounds (and (and (and (>= is_safe_row 0) (< is_safe_row is_safe_rows)) (>= is_safe_col 0)) (< is_safe_col is_safe_cols))) (set! is_safe_not_visited (= is_safe_visited_cell false)) (throw (ex-info "return" {:v (and (and is_safe_within_bounds is_safe_not_visited) (= (nth (nth is_safe_grid is_safe_row) is_safe_col) 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dfs [dfs_grid dfs_visited_p dfs_row dfs_col]
  (binding [dfs_col_nbr nil dfs_k nil dfs_new_col nil dfs_new_row nil dfs_row_nbr nil dfs_visited nil] (do (set! dfs_visited dfs_visited_p) (set! dfs_row_nbr [(- 1) (- 1) (- 1) 0 0 1 1 1]) (set! dfs_col_nbr [(- 1) 0 1 (- 1) 1 (- 1) 0 1]) (set! dfs_visited (assoc-in dfs_visited [dfs_row dfs_col] true)) (set! dfs_k 0) (while (< dfs_k 8) (do (set! dfs_new_row (+ dfs_row (nth dfs_row_nbr dfs_k))) (set! dfs_new_col (+ dfs_col (nth dfs_col_nbr dfs_k))) (when (is_safe dfs_grid dfs_visited dfs_new_row dfs_new_col) (dfs dfs_grid dfs_visited dfs_new_row dfs_new_col)) (set! dfs_k (+ dfs_k 1)))) dfs_grid)))

(defn count_islands [count_islands_grid]
  (binding [count_islands_cols nil count_islands_i nil count_islands_j nil count_islands_row_list nil count_islands_rows nil count_islands_visited nil count_v nil] (try (do (set! count_islands_rows (count count_islands_grid)) (set! count_islands_cols (count (nth count_islands_grid 0))) (set! count_islands_visited []) (set! count_islands_i 0) (while (< count_islands_i count_islands_rows) (do (set! count_islands_row_list []) (set! count_islands_j 0) (while (< count_islands_j count_islands_cols) (do (set! count_islands_row_list (conj count_islands_row_list false)) (set! count_islands_j (+ count_islands_j 1)))) (set! count_islands_visited (conj count_islands_visited count_islands_row_list)) (set! count_islands_i (+ count_islands_i 1)))) (set! count_v 0) (set! count_islands_i 0) (while (< count_islands_i count_islands_rows) (do (set! count_islands_j 0) (while (< count_islands_j count_islands_cols) (do (when (and (not (nth (nth count_islands_visited count_islands_i) count_islands_j)) (= (nth (nth count_islands_grid count_islands_i) count_islands_j) 1)) (do (dfs count_islands_grid count_islands_visited count_islands_i count_islands_j) (set! count_v (+ count_v 1)))) (set! count_islands_j (+ count_islands_j 1)))) (set! count_islands_i (+ count_islands_i 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_grid [[1 1 0 0 0] [0 1 0 0 1] [1 0 0 1 1] [0 0 0 0 0] [1 0 1 0 1]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (count_islands main_grid))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
