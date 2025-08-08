(ns main (:refer-clojure :exclude [create_bool_matrix is_sum_subset]))

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

(declare create_bool_matrix is_sum_subset)

(def ^:dynamic create_bool_matrix_i nil)

(def ^:dynamic create_bool_matrix_j nil)

(def ^:dynamic create_bool_matrix_matrix nil)

(def ^:dynamic create_bool_matrix_row nil)

(def ^:dynamic is_sum_subset_arr_len nil)

(def ^:dynamic is_sum_subset_i nil)

(def ^:dynamic is_sum_subset_j nil)

(def ^:dynamic is_sum_subset_subset nil)

(defn create_bool_matrix [create_bool_matrix_rows create_bool_matrix_cols]
  (binding [create_bool_matrix_i nil create_bool_matrix_j nil create_bool_matrix_matrix nil create_bool_matrix_row nil] (try (do (set! create_bool_matrix_matrix []) (set! create_bool_matrix_i 0) (while (<= create_bool_matrix_i create_bool_matrix_rows) (do (set! create_bool_matrix_row []) (set! create_bool_matrix_j 0) (while (<= create_bool_matrix_j create_bool_matrix_cols) (do (set! create_bool_matrix_row (conj create_bool_matrix_row false)) (set! create_bool_matrix_j (+ create_bool_matrix_j 1)))) (set! create_bool_matrix_matrix (conj create_bool_matrix_matrix create_bool_matrix_row)) (set! create_bool_matrix_i (+ create_bool_matrix_i 1)))) (throw (ex-info "return" {:v create_bool_matrix_matrix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_sum_subset [is_sum_subset_arr is_sum_subset_required_sum]
  (binding [is_sum_subset_arr_len nil is_sum_subset_i nil is_sum_subset_j nil is_sum_subset_subset nil] (try (do (set! is_sum_subset_arr_len (count is_sum_subset_arr)) (set! is_sum_subset_subset (create_bool_matrix is_sum_subset_arr_len is_sum_subset_required_sum)) (set! is_sum_subset_i 0) (while (<= is_sum_subset_i is_sum_subset_arr_len) (do (set! is_sum_subset_subset (assoc-in is_sum_subset_subset [is_sum_subset_i 0] true)) (set! is_sum_subset_i (+ is_sum_subset_i 1)))) (set! is_sum_subset_j 1) (while (<= is_sum_subset_j is_sum_subset_required_sum) (do (set! is_sum_subset_subset (assoc-in is_sum_subset_subset [0 is_sum_subset_j] false)) (set! is_sum_subset_j (+ is_sum_subset_j 1)))) (set! is_sum_subset_i 1) (while (<= is_sum_subset_i is_sum_subset_arr_len) (do (set! is_sum_subset_j 1) (while (<= is_sum_subset_j is_sum_subset_required_sum) (do (when (> (nth is_sum_subset_arr (- is_sum_subset_i 1)) is_sum_subset_j) (set! is_sum_subset_subset (assoc-in is_sum_subset_subset [is_sum_subset_i is_sum_subset_j] (nth (nth is_sum_subset_subset (- is_sum_subset_i 1)) is_sum_subset_j)))) (when (<= (nth is_sum_subset_arr (- is_sum_subset_i 1)) is_sum_subset_j) (set! is_sum_subset_subset (assoc-in is_sum_subset_subset [is_sum_subset_i is_sum_subset_j] (or (nth (nth is_sum_subset_subset (- is_sum_subset_i 1)) is_sum_subset_j) (nth (nth is_sum_subset_subset (- is_sum_subset_i 1)) (- is_sum_subset_j (nth is_sum_subset_arr (- is_sum_subset_i 1)))))))) (set! is_sum_subset_j (+ is_sum_subset_j 1)))) (set! is_sum_subset_i (+ is_sum_subset_i 1)))) (throw (ex-info "return" {:v (nth (nth is_sum_subset_subset is_sum_subset_arr_len) is_sum_subset_required_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (is_sum_subset [2 4 6 8] 5))
      (println (is_sum_subset [2 4 6 8] 14))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
