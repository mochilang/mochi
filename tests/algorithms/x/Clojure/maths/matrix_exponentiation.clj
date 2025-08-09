(ns main (:refer-clojure :exclude [identity matrix_mul matrix_pow fibonacci_with_matrix_exponentiation simple_fibonacci]))

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

(declare identity matrix_mul matrix_pow fibonacci_with_matrix_exponentiation simple_fibonacci)

(def ^:dynamic count_v nil)

(def ^:dynamic fibonacci_with_matrix_exponentiation_base nil)

(def ^:dynamic fibonacci_with_matrix_exponentiation_m nil)

(def ^:dynamic identity_i nil)

(def ^:dynamic identity_j nil)

(def ^:dynamic identity_mat nil)

(def ^:dynamic identity_row nil)

(def ^:dynamic matrix_mul_cell nil)

(def ^:dynamic matrix_mul_i nil)

(def ^:dynamic matrix_mul_j nil)

(def ^:dynamic matrix_mul_k nil)

(def ^:dynamic matrix_mul_n nil)

(def ^:dynamic matrix_mul_result nil)

(def ^:dynamic matrix_mul_row nil)

(def ^:dynamic matrix_pow_b nil)

(def ^:dynamic matrix_pow_e nil)

(def ^:dynamic matrix_pow_result nil)

(def ^:dynamic simple_fibonacci_a nil)

(def ^:dynamic simple_fibonacci_b nil)

(def ^:dynamic simple_fibonacci_tmp nil)

(defn identity [identity_n]
  (binding [identity_i nil identity_j nil identity_mat nil identity_row nil] (try (do (set! identity_i 0) (set! identity_mat []) (while (< identity_i identity_n) (do (set! identity_row []) (set! identity_j 0) (while (< identity_j identity_n) (do (if (= identity_i identity_j) (set! identity_row (conj identity_row 1)) (set! identity_row (conj identity_row 0))) (set! identity_j (+ identity_j 1)))) (set! identity_mat (conj identity_mat identity_row)) (set! identity_i (+ identity_i 1)))) (throw (ex-info "return" {:v identity_mat}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_mul [matrix_mul_a matrix_mul_b]
  (binding [matrix_mul_cell nil matrix_mul_i nil matrix_mul_j nil matrix_mul_k nil matrix_mul_n nil matrix_mul_result nil matrix_mul_row nil] (try (do (set! matrix_mul_n (count matrix_mul_a)) (set! matrix_mul_result []) (set! matrix_mul_i 0) (while (< matrix_mul_i matrix_mul_n) (do (set! matrix_mul_row []) (set! matrix_mul_j 0) (while (< matrix_mul_j matrix_mul_n) (do (set! matrix_mul_cell 0) (set! matrix_mul_k 0) (while (< matrix_mul_k matrix_mul_n) (do (set! matrix_mul_cell (+ matrix_mul_cell (* (nth (nth matrix_mul_a matrix_mul_i) matrix_mul_k) (nth (nth matrix_mul_b matrix_mul_k) matrix_mul_j)))) (set! matrix_mul_k (+ matrix_mul_k 1)))) (set! matrix_mul_row (conj matrix_mul_row matrix_mul_cell)) (set! matrix_mul_j (+ matrix_mul_j 1)))) (set! matrix_mul_result (conj matrix_mul_result matrix_mul_row)) (set! matrix_mul_i (+ matrix_mul_i 1)))) (throw (ex-info "return" {:v matrix_mul_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_pow [matrix_pow_base matrix_pow_exp]
  (binding [matrix_pow_b nil matrix_pow_e nil matrix_pow_result nil] (try (do (set! matrix_pow_result (identity (count matrix_pow_base))) (set! matrix_pow_b matrix_pow_base) (set! matrix_pow_e matrix_pow_exp) (while (> matrix_pow_e 0) (do (when (= (mod matrix_pow_e 2) 1) (set! matrix_pow_result (matrix_mul matrix_pow_result matrix_pow_b))) (set! matrix_pow_b (matrix_mul matrix_pow_b matrix_pow_b)) (set! matrix_pow_e (quot matrix_pow_e 2)))) (throw (ex-info "return" {:v matrix_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fibonacci_with_matrix_exponentiation [fibonacci_with_matrix_exponentiation_n fibonacci_with_matrix_exponentiation_f1 fibonacci_with_matrix_exponentiation_f2]
  (binding [fibonacci_with_matrix_exponentiation_base nil fibonacci_with_matrix_exponentiation_m nil] (try (do (when (= fibonacci_with_matrix_exponentiation_n 1) (throw (ex-info "return" {:v fibonacci_with_matrix_exponentiation_f1}))) (when (= fibonacci_with_matrix_exponentiation_n 2) (throw (ex-info "return" {:v fibonacci_with_matrix_exponentiation_f2}))) (set! fibonacci_with_matrix_exponentiation_base [[1 1] [1 0]]) (set! fibonacci_with_matrix_exponentiation_m (matrix_pow fibonacci_with_matrix_exponentiation_base (- fibonacci_with_matrix_exponentiation_n 2))) (throw (ex-info "return" {:v (+ (* fibonacci_with_matrix_exponentiation_f2 (nth (nth fibonacci_with_matrix_exponentiation_m 0) 0)) (* fibonacci_with_matrix_exponentiation_f1 (nth (nth fibonacci_with_matrix_exponentiation_m 0) 1)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn simple_fibonacci [simple_fibonacci_n simple_fibonacci_f1 simple_fibonacci_f2]
  (binding [count_v nil simple_fibonacci_a nil simple_fibonacci_b nil simple_fibonacci_tmp nil] (try (do (when (= simple_fibonacci_n 1) (throw (ex-info "return" {:v simple_fibonacci_f1}))) (when (= simple_fibonacci_n 2) (throw (ex-info "return" {:v simple_fibonacci_f2}))) (set! simple_fibonacci_a simple_fibonacci_f1) (set! simple_fibonacci_b simple_fibonacci_f2) (set! count_v (- simple_fibonacci_n 2)) (while (> count_v 0) (do (set! simple_fibonacci_tmp (+ simple_fibonacci_a simple_fibonacci_b)) (set! simple_fibonacci_a simple_fibonacci_b) (set! simple_fibonacci_b simple_fibonacci_tmp) (set! count_v (- count_v 1)))) (throw (ex-info "return" {:v simple_fibonacci_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (fibonacci_with_matrix_exponentiation 1 5 6)))
      (println (str (fibonacci_with_matrix_exponentiation 2 10 11)))
      (println (str (fibonacci_with_matrix_exponentiation 13 0 1)))
      (println (str (fibonacci_with_matrix_exponentiation 10 5 9)))
      (println (str (fibonacci_with_matrix_exponentiation 9 2 3)))
      (println (str (simple_fibonacci 1 5 6)))
      (println (str (simple_fibonacci 2 10 11)))
      (println (str (simple_fibonacci 13 0 1)))
      (println (str (simple_fibonacci 10 5 9)))
      (println (str (simple_fibonacci 9 2 3)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
