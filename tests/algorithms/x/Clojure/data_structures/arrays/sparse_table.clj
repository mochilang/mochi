(ns main (:refer-clojure :exclude [pow2 int_log2 build_sparse_table query]))

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

(declare pow2 int_log2 build_sparse_table query)

(def ^:dynamic build_sparse_table_i nil)

(def ^:dynamic build_sparse_table_inner nil)

(def ^:dynamic build_sparse_table_j nil)

(def ^:dynamic build_sparse_table_left nil)

(def ^:dynamic build_sparse_table_length nil)

(def ^:dynamic build_sparse_table_right nil)

(def ^:dynamic build_sparse_table_row nil)

(def ^:dynamic build_sparse_table_sparse_table nil)

(def ^:dynamic int_log2_res nil)

(def ^:dynamic int_log2_v nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_result nil)

(def ^:dynamic query_interval nil)

(def ^:dynamic query_j nil)

(def ^:dynamic query_val1 nil)

(def ^:dynamic query_val2 nil)

(defn pow2 [pow2_n]
  (binding [pow2_i nil pow2_result nil] (try (do (set! pow2_result 1) (set! pow2_i 0) (while (< pow2_i pow2_n) (do (set! pow2_result (* pow2_result 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn int_log2 [int_log2_n]
  (binding [int_log2_res nil int_log2_v nil] (try (do (set! int_log2_v int_log2_n) (set! int_log2_res 0) (while (> int_log2_v 1) (do (set! int_log2_v (quot int_log2_v 2)) (set! int_log2_res (+ int_log2_res 1)))) (throw (ex-info "return" {:v int_log2_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn build_sparse_table [build_sparse_table_number_list]
  (binding [build_sparse_table_i nil build_sparse_table_inner nil build_sparse_table_j nil build_sparse_table_left nil build_sparse_table_length nil build_sparse_table_right nil build_sparse_table_row nil build_sparse_table_sparse_table nil] (try (do (when (= (count build_sparse_table_number_list) 0) (throw (Exception. "empty number list not allowed"))) (set! build_sparse_table_length (count build_sparse_table_number_list)) (set! build_sparse_table_row (+ (int_log2 build_sparse_table_length) 1)) (set! build_sparse_table_sparse_table []) (set! build_sparse_table_j 0) (while (< build_sparse_table_j build_sparse_table_row) (do (set! build_sparse_table_inner []) (set! build_sparse_table_i 0) (while (< build_sparse_table_i build_sparse_table_length) (do (set! build_sparse_table_inner (conj build_sparse_table_inner 0)) (set! build_sparse_table_i (+ build_sparse_table_i 1)))) (set! build_sparse_table_sparse_table (conj build_sparse_table_sparse_table build_sparse_table_inner)) (set! build_sparse_table_j (+ build_sparse_table_j 1)))) (set! build_sparse_table_i 0) (while (< build_sparse_table_i build_sparse_table_length) (do (set! build_sparse_table_sparse_table (assoc-in build_sparse_table_sparse_table [0 build_sparse_table_i] (nth build_sparse_table_number_list build_sparse_table_i))) (set! build_sparse_table_i (+ build_sparse_table_i 1)))) (set! build_sparse_table_j 1) (while (<= (pow2 build_sparse_table_j) build_sparse_table_length) (do (set! build_sparse_table_i 0) (while (< (- (+ build_sparse_table_i (pow2 build_sparse_table_j)) 1) build_sparse_table_length) (do (set! build_sparse_table_left (nth (nth build_sparse_table_sparse_table (- build_sparse_table_j 1)) (+ build_sparse_table_i (pow2 (- build_sparse_table_j 1))))) (set! build_sparse_table_right (nth (nth build_sparse_table_sparse_table (- build_sparse_table_j 1)) build_sparse_table_i)) (if (< build_sparse_table_left build_sparse_table_right) (set! build_sparse_table_sparse_table (assoc-in build_sparse_table_sparse_table [build_sparse_table_j build_sparse_table_i] build_sparse_table_left)) (set! build_sparse_table_sparse_table (assoc-in build_sparse_table_sparse_table [build_sparse_table_j build_sparse_table_i] build_sparse_table_right))) (set! build_sparse_table_i (+ build_sparse_table_i 1)))) (set! build_sparse_table_j (+ build_sparse_table_j 1)))) (throw (ex-info "return" {:v build_sparse_table_sparse_table}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn query [query_sparse_table query_left_bound query_right_bound]
  (binding [query_interval nil query_j nil query_val1 nil query_val2 nil] (try (do (when (or (< query_left_bound 0) (>= query_right_bound (count (nth query_sparse_table 0)))) (throw (Exception. "list index out of range"))) (set! query_interval (+ (- query_right_bound query_left_bound) 1)) (set! query_j (int_log2 query_interval)) (set! query_val1 (nth (nth query_sparse_table query_j) (+ (- query_right_bound (pow2 query_j)) 1))) (set! query_val2 (nth (nth query_sparse_table query_j) query_left_bound)) (if (< query_val1 query_val2) query_val1 query_val2)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_st1 (build_sparse_table [8 1 0 3 4 9 3]))

(def ^:dynamic main_st2 (build_sparse_table [3 1 9]))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_st1))
      (println (str main_st2))
      (println (str (query main_st1 0 4)))
      (println (str (query main_st1 4 6)))
      (println (str (query main_st2 2 2)))
      (println (str (query main_st2 0 1)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
