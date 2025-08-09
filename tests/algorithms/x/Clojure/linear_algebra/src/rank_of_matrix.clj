(ns main (:refer-clojure :exclude [rank_of_matrix]))

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

(declare rank_of_matrix)

(def ^:dynamic rank_of_matrix_col nil)

(def ^:dynamic rank_of_matrix_columns nil)

(def ^:dynamic rank_of_matrix_i nil)

(def ^:dynamic rank_of_matrix_j nil)

(def ^:dynamic rank_of_matrix_matrix nil)

(def ^:dynamic rank_of_matrix_mult nil)

(def ^:dynamic rank_of_matrix_rank nil)

(def ^:dynamic rank_of_matrix_reduce nil)

(def ^:dynamic rank_of_matrix_row nil)

(def ^:dynamic rank_of_matrix_rows nil)

(def ^:dynamic rank_of_matrix_temp nil)

(defn rank_of_matrix [rank_of_matrix_matrix_p]
  (binding [rank_of_matrix_col nil rank_of_matrix_columns nil rank_of_matrix_i nil rank_of_matrix_j nil rank_of_matrix_matrix nil rank_of_matrix_mult nil rank_of_matrix_rank nil rank_of_matrix_reduce nil rank_of_matrix_row nil rank_of_matrix_rows nil rank_of_matrix_temp nil] (try (do (set! rank_of_matrix_matrix rank_of_matrix_matrix_p) (set! rank_of_matrix_rows (count rank_of_matrix_matrix)) (when (= rank_of_matrix_rows 0) (throw (ex-info "return" {:v 0}))) (set! rank_of_matrix_columns (if (> (count (nth rank_of_matrix_matrix 0)) 0) (count (nth rank_of_matrix_matrix 0)) 0)) (set! rank_of_matrix_rank (if (< rank_of_matrix_rows rank_of_matrix_columns) rank_of_matrix_rows rank_of_matrix_columns)) (set! rank_of_matrix_row 0) (while (< rank_of_matrix_row rank_of_matrix_rank) (do (if (not= (nth (nth rank_of_matrix_matrix rank_of_matrix_row) rank_of_matrix_row) 0.0) (do (set! rank_of_matrix_col (+ rank_of_matrix_row 1)) (while (< rank_of_matrix_col rank_of_matrix_rows) (do (set! rank_of_matrix_mult (quot (nth (nth rank_of_matrix_matrix rank_of_matrix_col) rank_of_matrix_row) (nth (nth rank_of_matrix_matrix rank_of_matrix_row) rank_of_matrix_row))) (set! rank_of_matrix_i rank_of_matrix_row) (while (< rank_of_matrix_i rank_of_matrix_columns) (do (set! rank_of_matrix_matrix (assoc-in rank_of_matrix_matrix [rank_of_matrix_col rank_of_matrix_i] (- (nth (nth rank_of_matrix_matrix rank_of_matrix_col) rank_of_matrix_i) (* rank_of_matrix_mult (nth (nth rank_of_matrix_matrix rank_of_matrix_row) rank_of_matrix_i))))) (set! rank_of_matrix_i (+ rank_of_matrix_i 1)))) (set! rank_of_matrix_col (+ rank_of_matrix_col 1))))) (do (set! rank_of_matrix_reduce true) (set! rank_of_matrix_i (+ rank_of_matrix_row 1)) (loop [while_flag_1 true] (when (and while_flag_1 (< rank_of_matrix_i rank_of_matrix_rows)) (cond (not= (nth (nth rank_of_matrix_matrix rank_of_matrix_i) rank_of_matrix_row) 0.0) (do (set! rank_of_matrix_temp (nth rank_of_matrix_matrix rank_of_matrix_row)) (set! rank_of_matrix_matrix (assoc rank_of_matrix_matrix rank_of_matrix_row (nth rank_of_matrix_matrix rank_of_matrix_i))) (set! rank_of_matrix_matrix (assoc rank_of_matrix_matrix rank_of_matrix_i rank_of_matrix_temp)) (set! rank_of_matrix_reduce false) (recur false)) :else (do (set! rank_of_matrix_i (+ rank_of_matrix_i 1)) (recur while_flag_1))))) (when rank_of_matrix_reduce (do (set! rank_of_matrix_rank (- rank_of_matrix_rank 1)) (set! rank_of_matrix_j 0) (while (< rank_of_matrix_j rank_of_matrix_rows) (do (set! rank_of_matrix_matrix (assoc-in rank_of_matrix_matrix [rank_of_matrix_j rank_of_matrix_row] (nth (nth rank_of_matrix_matrix rank_of_matrix_j) rank_of_matrix_rank))) (set! rank_of_matrix_j (+ rank_of_matrix_j 1)))))) (set! rank_of_matrix_row (- rank_of_matrix_row 1)))) (set! rank_of_matrix_row (+ rank_of_matrix_row 1)))) (throw (ex-info "return" {:v rank_of_matrix_rank}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
