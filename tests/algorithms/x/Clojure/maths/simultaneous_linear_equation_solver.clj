(ns main (:refer-clojure :exclude [floor pow10 round clone_matrix solve_simultaneous test_solver main]))

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

(declare floor pow10 round clone_matrix solve_simultaneous test_solver main)

(def ^:dynamic clone_matrix_i nil)

(def ^:dynamic clone_matrix_j nil)

(def ^:dynamic clone_matrix_new_mat nil)

(def ^:dynamic clone_matrix_row nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic main_eq nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_p nil)

(def ^:dynamic round_m nil)

(def ^:dynamic solve_simultaneous_a nil)

(def ^:dynamic solve_simultaneous_c nil)

(def ^:dynamic solve_simultaneous_col nil)

(def ^:dynamic solve_simultaneous_factor nil)

(def ^:dynamic solve_simultaneous_i nil)

(def ^:dynamic solve_simultaneous_k nil)

(def ^:dynamic solve_simultaneous_m nil)

(def ^:dynamic solve_simultaneous_n nil)

(def ^:dynamic solve_simultaneous_pivot nil)

(def ^:dynamic solve_simultaneous_pivot_val nil)

(def ^:dynamic solve_simultaneous_r nil)

(def ^:dynamic solve_simultaneous_res nil)

(def ^:dynamic solve_simultaneous_row nil)

(def ^:dynamic solve_simultaneous_temp nil)

(def ^:dynamic test_solver_a nil)

(def ^:dynamic test_solver_b nil)

(def ^:dynamic test_solver_r1 nil)

(def ^:dynamic test_solver_r2 nil)

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_p nil] (try (do (set! pow10_p 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_p (* pow10_p 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round [round_x round_n]
  (binding [round_m nil] (try (do (set! round_m (pow10 round_n)) (throw (ex-info "return" {:v (/ (floor (+ (* round_x round_m) 0.5)) round_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn clone_matrix [clone_matrix_mat]
  (binding [clone_matrix_i nil clone_matrix_j nil clone_matrix_new_mat nil clone_matrix_row nil] (try (do (set! clone_matrix_new_mat []) (set! clone_matrix_i 0) (while (< clone_matrix_i (count clone_matrix_mat)) (do (set! clone_matrix_row []) (set! clone_matrix_j 0) (while (< clone_matrix_j (count (nth clone_matrix_mat clone_matrix_i))) (do (set! clone_matrix_row (conj clone_matrix_row (nth (nth clone_matrix_mat clone_matrix_i) clone_matrix_j))) (set! clone_matrix_j (+ clone_matrix_j 1)))) (set! clone_matrix_new_mat (conj clone_matrix_new_mat clone_matrix_row)) (set! clone_matrix_i (+ clone_matrix_i 1)))) (throw (ex-info "return" {:v clone_matrix_new_mat}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solve_simultaneous [solve_simultaneous_equations]
  (binding [solve_simultaneous_a nil solve_simultaneous_c nil solve_simultaneous_col nil solve_simultaneous_factor nil solve_simultaneous_i nil solve_simultaneous_k nil solve_simultaneous_m nil solve_simultaneous_n nil solve_simultaneous_pivot nil solve_simultaneous_pivot_val nil solve_simultaneous_r nil solve_simultaneous_res nil solve_simultaneous_row nil solve_simultaneous_temp nil] (try (do (set! solve_simultaneous_n (count solve_simultaneous_equations)) (when (= solve_simultaneous_n 0) (throw (Exception. "solve_simultaneous() requires n lists of length n+1"))) (set! solve_simultaneous_m (+ solve_simultaneous_n 1)) (set! solve_simultaneous_i 0) (while (< solve_simultaneous_i solve_simultaneous_n) (do (when (not= (count (nth solve_simultaneous_equations solve_simultaneous_i)) solve_simultaneous_m) (throw (Exception. "solve_simultaneous() requires n lists of length n+1"))) (set! solve_simultaneous_i (+ solve_simultaneous_i 1)))) (set! solve_simultaneous_a (clone_matrix solve_simultaneous_equations)) (set! solve_simultaneous_row 0) (while (< solve_simultaneous_row solve_simultaneous_n) (do (set! solve_simultaneous_pivot solve_simultaneous_row) (while (and (< solve_simultaneous_pivot solve_simultaneous_n) (= (nth (nth solve_simultaneous_a solve_simultaneous_pivot) solve_simultaneous_row) 0.0)) (set! solve_simultaneous_pivot (+ solve_simultaneous_pivot 1))) (when (= solve_simultaneous_pivot solve_simultaneous_n) (throw (Exception. "solve_simultaneous() requires at least 1 full equation"))) (when (not= solve_simultaneous_pivot solve_simultaneous_row) (do (set! solve_simultaneous_temp (nth solve_simultaneous_a solve_simultaneous_row)) (set! solve_simultaneous_a (assoc solve_simultaneous_a solve_simultaneous_row (nth solve_simultaneous_a solve_simultaneous_pivot))) (set! solve_simultaneous_a (assoc solve_simultaneous_a solve_simultaneous_pivot solve_simultaneous_temp)))) (set! solve_simultaneous_pivot_val (nth (nth solve_simultaneous_a solve_simultaneous_row) solve_simultaneous_row)) (set! solve_simultaneous_col 0) (while (< solve_simultaneous_col solve_simultaneous_m) (do (set! solve_simultaneous_a (assoc-in solve_simultaneous_a [solve_simultaneous_row solve_simultaneous_col] (quot (nth (nth solve_simultaneous_a solve_simultaneous_row) solve_simultaneous_col) solve_simultaneous_pivot_val))) (set! solve_simultaneous_col (+ solve_simultaneous_col 1)))) (set! solve_simultaneous_r 0) (while (< solve_simultaneous_r solve_simultaneous_n) (do (when (not= solve_simultaneous_r solve_simultaneous_row) (do (set! solve_simultaneous_factor (nth (nth solve_simultaneous_a solve_simultaneous_r) solve_simultaneous_row)) (set! solve_simultaneous_c 0) (while (< solve_simultaneous_c solve_simultaneous_m) (do (set! solve_simultaneous_a (assoc-in solve_simultaneous_a [solve_simultaneous_r solve_simultaneous_c] (- (nth (nth solve_simultaneous_a solve_simultaneous_r) solve_simultaneous_c) (* solve_simultaneous_factor (nth (nth solve_simultaneous_a solve_simultaneous_row) solve_simultaneous_c))))) (set! solve_simultaneous_c (+ solve_simultaneous_c 1)))))) (set! solve_simultaneous_r (+ solve_simultaneous_r 1)))) (set! solve_simultaneous_row (+ solve_simultaneous_row 1)))) (set! solve_simultaneous_res []) (set! solve_simultaneous_k 0) (while (< solve_simultaneous_k solve_simultaneous_n) (do (set! solve_simultaneous_res (conj solve_simultaneous_res (round (nth (nth solve_simultaneous_a solve_simultaneous_k) (- solve_simultaneous_m 1)) 5))) (set! solve_simultaneous_k (+ solve_simultaneous_k 1)))) (throw (ex-info "return" {:v solve_simultaneous_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_solver []
  (binding [test_solver_a nil test_solver_b nil test_solver_r1 nil test_solver_r2 nil] (do (set! test_solver_a [[1.0 2.0 3.0] [4.0 5.0 6.0]]) (set! test_solver_r1 (solve_simultaneous test_solver_a)) (when (not (and (and (= (count test_solver_r1) 2) (= (nth test_solver_r1 0) (- 0.0 1.0))) (= (nth test_solver_r1 1) 2.0))) (throw (Exception. "test1 failed"))) (set! test_solver_b [[0.0 (- 0.0 3.0) 1.0 7.0] [3.0 2.0 (- 0.0 1.0) 11.0] [5.0 1.0 (- 0.0 2.0) 12.0]]) (set! test_solver_r2 (solve_simultaneous test_solver_b)) (when (not (and (and (and (= (count test_solver_r2) 3) (= (nth test_solver_r2 0) 6.4)) (= (nth test_solver_r2 1) 1.2)) (= (nth test_solver_r2 2) 10.6))) (throw (Exception. "test2 failed"))))))

(defn main []
  (binding [main_eq nil] (do (test_solver) (set! main_eq [[2.0 1.0 1.0 1.0 1.0 4.0] [1.0 2.0 1.0 1.0 1.0 5.0] [1.0 1.0 2.0 1.0 1.0 6.0] [1.0 1.0 1.0 2.0 1.0 7.0] [1.0 1.0 1.0 1.0 2.0 8.0]]) (println (str (solve_simultaneous main_eq))) (println (str (solve_simultaneous [[4.0 2.0]]))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
