(ns main (:refer-clojure :exclude [panic abs_float copy_matrix solve_linear_system]))

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

(declare panic abs_float copy_matrix solve_linear_system)

(def ^:dynamic copy_matrix_i nil)

(def ^:dynamic copy_matrix_j nil)

(def ^:dynamic copy_matrix_res nil)

(def ^:dynamic copy_matrix_row nil)

(def ^:dynamic copy_matrix_row_src nil)

(def ^:dynamic solve_linear_system_ab nil)

(def ^:dynamic solve_linear_system_column_num nil)

(def ^:dynamic solve_linear_system_factor nil)

(def ^:dynamic solve_linear_system_i nil)

(def ^:dynamic solve_linear_system_j nil)

(def ^:dynamic solve_linear_system_num_cols nil)

(def ^:dynamic solve_linear_system_num_rows nil)

(def ^:dynamic solve_linear_system_t nil)

(def ^:dynamic solve_linear_system_temp nil)

(def ^:dynamic solve_linear_system_x nil)

(def ^:dynamic solve_linear_system_x_lst nil)

(defn panic [panic_msg]
  (do (println panic_msg) panic_msg))

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (- abs_float_x) abs_float_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn copy_matrix [copy_matrix_src]
  (binding [copy_matrix_i nil copy_matrix_j nil copy_matrix_res nil copy_matrix_row nil copy_matrix_row_src nil] (try (do (set! copy_matrix_res []) (set! copy_matrix_i 0) (while (< copy_matrix_i (count copy_matrix_src)) (do (set! copy_matrix_row_src (nth copy_matrix_src copy_matrix_i)) (set! copy_matrix_row []) (set! copy_matrix_j 0) (while (< copy_matrix_j (count copy_matrix_row_src)) (do (set! copy_matrix_row (conj copy_matrix_row (nth copy_matrix_row_src copy_matrix_j))) (set! copy_matrix_j (+ copy_matrix_j 1)))) (set! copy_matrix_res (conj copy_matrix_res copy_matrix_row)) (set! copy_matrix_i (+ copy_matrix_i 1)))) (throw (ex-info "return" {:v copy_matrix_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solve_linear_system [solve_linear_system_matrix]
  (binding [solve_linear_system_ab nil solve_linear_system_column_num nil solve_linear_system_factor nil solve_linear_system_i nil solve_linear_system_j nil solve_linear_system_num_cols nil solve_linear_system_num_rows nil solve_linear_system_t nil solve_linear_system_temp nil solve_linear_system_x nil solve_linear_system_x_lst nil] (try (do (set! solve_linear_system_ab (copy_matrix solve_linear_system_matrix)) (set! solve_linear_system_num_rows (count solve_linear_system_ab)) (set! solve_linear_system_num_cols (- (count (nth solve_linear_system_ab 0)) 1)) (when (not= solve_linear_system_num_rows solve_linear_system_num_cols) (do (panic "Matrix is not square") (throw (ex-info "return" {:v []})))) (set! solve_linear_system_column_num 0) (while (< solve_linear_system_column_num solve_linear_system_num_rows) (do (set! solve_linear_system_i solve_linear_system_column_num) (while (< solve_linear_system_i solve_linear_system_num_cols) (do (when (> (abs_float (nth (nth solve_linear_system_ab solve_linear_system_i) solve_linear_system_column_num)) (abs_float (nth (nth solve_linear_system_ab solve_linear_system_column_num) solve_linear_system_column_num))) (do (set! solve_linear_system_temp (nth solve_linear_system_ab solve_linear_system_column_num)) (set! solve_linear_system_ab (assoc solve_linear_system_ab solve_linear_system_column_num (nth solve_linear_system_ab solve_linear_system_i))) (set! solve_linear_system_ab (assoc solve_linear_system_ab solve_linear_system_i solve_linear_system_temp)))) (set! solve_linear_system_i (+ solve_linear_system_i 1)))) (when (< (abs_float (nth (nth solve_linear_system_ab solve_linear_system_column_num) solve_linear_system_column_num)) 0.00000001) (do (panic "Matrix is singular") (throw (ex-info "return" {:v []})))) (when (not= solve_linear_system_column_num 0) (do (set! solve_linear_system_i solve_linear_system_column_num) (while (< solve_linear_system_i solve_linear_system_num_rows) (do (set! solve_linear_system_factor (quot (nth (nth solve_linear_system_ab solve_linear_system_i) (- solve_linear_system_column_num 1)) (nth (nth solve_linear_system_ab (- solve_linear_system_column_num 1)) (- solve_linear_system_column_num 1)))) (set! solve_linear_system_j 0) (while (< solve_linear_system_j (count (nth solve_linear_system_ab solve_linear_system_i))) (do (set! solve_linear_system_ab (assoc-in solve_linear_system_ab [solve_linear_system_i solve_linear_system_j] (- (nth (nth solve_linear_system_ab solve_linear_system_i) solve_linear_system_j) (* solve_linear_system_factor (nth (nth solve_linear_system_ab (- solve_linear_system_column_num 1)) solve_linear_system_j))))) (set! solve_linear_system_j (+ solve_linear_system_j 1)))) (set! solve_linear_system_i (+ solve_linear_system_i 1)))))) (set! solve_linear_system_column_num (+ solve_linear_system_column_num 1)))) (set! solve_linear_system_x_lst []) (set! solve_linear_system_t 0) (while (< solve_linear_system_t solve_linear_system_num_rows) (do (set! solve_linear_system_x_lst (conj solve_linear_system_x_lst 0.0)) (set! solve_linear_system_t (+ solve_linear_system_t 1)))) (set! solve_linear_system_column_num (- solve_linear_system_num_rows 1)) (while (>= solve_linear_system_column_num 0) (do (set! solve_linear_system_x (quot (nth (nth solve_linear_system_ab solve_linear_system_column_num) solve_linear_system_num_cols) (nth (nth solve_linear_system_ab solve_linear_system_column_num) solve_linear_system_column_num))) (set! solve_linear_system_x_lst (assoc solve_linear_system_x_lst solve_linear_system_column_num solve_linear_system_x)) (set! solve_linear_system_i (- solve_linear_system_column_num 1)) (while (>= solve_linear_system_i 0) (do (set! solve_linear_system_ab (assoc-in solve_linear_system_ab [solve_linear_system_i solve_linear_system_num_cols] (- (nth (nth solve_linear_system_ab solve_linear_system_i) solve_linear_system_num_cols) (* (nth (nth solve_linear_system_ab solve_linear_system_i) solve_linear_system_column_num) solve_linear_system_x)))) (set! solve_linear_system_i (- solve_linear_system_i 1)))) (set! solve_linear_system_column_num (- solve_linear_system_column_num 1)))) (throw (ex-info "return" {:v solve_linear_system_x_lst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example_matrix [[5.0 (- 5.0) (- 3.0) 4.0 (- 11.0)] [1.0 (- 4.0) 6.0 (- 4.0) (- 10.0)] [(- 2.0) (- 5.0) 4.0 (- 5.0) (- 12.0)] [(- 3.0) (- 3.0) 5.0 (- 5.0) 8.0]])

(def ^:dynamic main_solution (solve_linear_system main_example_matrix))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Matrix:")
      (println (str main_example_matrix))
      (println (str main_solution))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
