(ns main (:refer-clojure :exclude [sqrt powf roundf fib_iterative fib_recursive_term fib_recursive fib_recursive_cached_term fib_recursive_cached fib_memoization_term fib_memoization fib_binet matrix_mul matrix_pow fib_matrix run_tests]))

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

(declare sqrt powf roundf fib_iterative fib_recursive_term fib_recursive fib_recursive_cached_term fib_recursive_cached fib_memoization_term fib_memoization fib_binet matrix_mul matrix_pow fib_matrix run_tests)

(def ^:dynamic fib_binet_i nil)

(def ^:dynamic fib_binet_phi nil)

(def ^:dynamic fib_binet_res nil)

(def ^:dynamic fib_binet_sqrt5 nil)

(def ^:dynamic fib_binet_val nil)

(def ^:dynamic fib_iterative_fib nil)

(def ^:dynamic fib_iterative_i nil)

(def ^:dynamic fib_matrix_m nil)

(def ^:dynamic fib_matrix_res nil)

(def ^:dynamic fib_memoization_i nil)

(def ^:dynamic fib_memoization_out nil)

(def ^:dynamic fib_memoization_term_value nil)

(def ^:dynamic fib_recursive_cached_j nil)

(def ^:dynamic fib_recursive_cached_res nil)

(def ^:dynamic fib_recursive_cached_term_val nil)

(def ^:dynamic fib_recursive_i nil)

(def ^:dynamic fib_recursive_res nil)

(def ^:dynamic matrix_mul_a00 nil)

(def ^:dynamic matrix_mul_a01 nil)

(def ^:dynamic matrix_mul_a10 nil)

(def ^:dynamic matrix_mul_a11 nil)

(def ^:dynamic matrix_pow_base nil)

(def ^:dynamic matrix_pow_p nil)

(def ^:dynamic matrix_pow_result nil)

(def ^:dynamic powf_i nil)

(def ^:dynamic powf_res nil)

(def ^:dynamic run_tests_bin nil)

(def ^:dynamic run_tests_cache nil)

(def ^:dynamic run_tests_expected nil)

(def ^:dynamic run_tests_it nil)

(def ^:dynamic run_tests_m nil)

(def ^:dynamic run_tests_memo nil)

(def ^:dynamic run_tests_rec nil)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (when (<= sqrt_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_guess sqrt_x) (set! sqrt_i 0) (while (< sqrt_i 10) (do (set! sqrt_guess (/ (+ sqrt_guess (quot sqrt_x sqrt_guess)) 2.0)) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn powf [powf_x powf_n]
  (binding [powf_i nil powf_res nil] (try (do (set! powf_res 1.0) (set! powf_i 0) (while (< powf_i powf_n) (do (set! powf_res (* powf_res powf_x)) (set! powf_i (+ powf_i 1)))) (throw (ex-info "return" {:v powf_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn roundf [roundf_x]
  (try (if (>= roundf_x 0.0) (long (+ roundf_x 0.5)) (long (- roundf_x 0.5))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fib_iterative [fib_iterative_n]
  (binding [fib_iterative_fib nil fib_iterative_i nil] (try (do (when (< fib_iterative_n 0) (throw (Exception. "n is negative"))) (when (= fib_iterative_n 0) (throw (ex-info "return" {:v [0]}))) (set! fib_iterative_fib [0 1]) (set! fib_iterative_i 2) (while (<= fib_iterative_i fib_iterative_n) (do (set! fib_iterative_fib (conj fib_iterative_fib (+ (nth fib_iterative_fib (- fib_iterative_i 1)) (nth fib_iterative_fib (- fib_iterative_i 2))))) (set! fib_iterative_i (+ fib_iterative_i 1)))) (throw (ex-info "return" {:v fib_iterative_fib}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fib_recursive_term [fib_recursive_term_i]
  (try (do (when (< fib_recursive_term_i 0) (throw (Exception. "n is negative"))) (if (< fib_recursive_term_i 2) fib_recursive_term_i (+ (fib_recursive_term (- fib_recursive_term_i 1)) (fib_recursive_term (- fib_recursive_term_i 2))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fib_recursive [fib_recursive_n]
  (binding [fib_recursive_i nil fib_recursive_res nil] (try (do (when (< fib_recursive_n 0) (throw (Exception. "n is negative"))) (set! fib_recursive_res []) (set! fib_recursive_i 0) (while (<= fib_recursive_i fib_recursive_n) (do (set! fib_recursive_res (conj fib_recursive_res (fib_recursive_term fib_recursive_i))) (set! fib_recursive_i (+ fib_recursive_i 1)))) (throw (ex-info "return" {:v fib_recursive_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_fib_cache_global {})

(defn fib_recursive_cached_term [fib_recursive_cached_term_i]
  (binding [fib_recursive_cached_term_val nil] (try (do (when (< fib_recursive_cached_term_i 0) (throw (Exception. "n is negative"))) (when (< fib_recursive_cached_term_i 2) (throw (ex-info "return" {:v fib_recursive_cached_term_i}))) (when (in fib_recursive_cached_term_i main_fib_cache_global) (throw (ex-info "return" {:v (get main_fib_cache_global fib_recursive_cached_term_i)}))) (set! fib_recursive_cached_term_val (+ (fib_recursive_cached_term (- fib_recursive_cached_term_i 1)) (fib_recursive_cached_term (- fib_recursive_cached_term_i 2)))) (alter-var-root (var main_fib_cache_global) (fn [_] (assoc main_fib_cache_global fib_recursive_cached_term_i fib_recursive_cached_term_val))) (throw (ex-info "return" {:v fib_recursive_cached_term_val}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fib_recursive_cached [fib_recursive_cached_n]
  (binding [fib_recursive_cached_j nil fib_recursive_cached_res nil] (try (do (when (< fib_recursive_cached_n 0) (throw (Exception. "n is negative"))) (set! fib_recursive_cached_res []) (set! fib_recursive_cached_j 0) (while (<= fib_recursive_cached_j fib_recursive_cached_n) (do (set! fib_recursive_cached_res (conj fib_recursive_cached_res (fib_recursive_cached_term fib_recursive_cached_j))) (set! fib_recursive_cached_j (+ fib_recursive_cached_j 1)))) (throw (ex-info "return" {:v fib_recursive_cached_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_fib_memo_cache {0 0 1 1 2 1})

(defn fib_memoization_term [fib_memoization_term_num]
  (binding [fib_memoization_term_value nil] (try (do (when (in fib_memoization_term_num main_fib_memo_cache) (throw (ex-info "return" {:v (get main_fib_memo_cache fib_memoization_term_num)}))) (set! fib_memoization_term_value (+ (fib_memoization_term (- fib_memoization_term_num 1)) (fib_memoization_term (- fib_memoization_term_num 2)))) (alter-var-root (var main_fib_memo_cache) (fn [_] (assoc main_fib_memo_cache fib_memoization_term_num fib_memoization_term_value))) (throw (ex-info "return" {:v fib_memoization_term_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fib_memoization [fib_memoization_n]
  (binding [fib_memoization_i nil fib_memoization_out nil] (try (do (when (< fib_memoization_n 0) (throw (Exception. "n is negative"))) (set! fib_memoization_out []) (set! fib_memoization_i 0) (while (<= fib_memoization_i fib_memoization_n) (do (set! fib_memoization_out (conj fib_memoization_out (fib_memoization_term fib_memoization_i))) (set! fib_memoization_i (+ fib_memoization_i 1)))) (throw (ex-info "return" {:v fib_memoization_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fib_binet [fib_binet_n]
  (binding [fib_binet_i nil fib_binet_phi nil fib_binet_res nil fib_binet_sqrt5 nil fib_binet_val nil] (try (do (when (< fib_binet_n 0) (throw (Exception. "n is negative"))) (when (>= fib_binet_n 1475) (throw (Exception. "n is too large"))) (set! fib_binet_sqrt5 (sqrt 5.0)) (set! fib_binet_phi (/ (+ 1.0 fib_binet_sqrt5) 2.0)) (set! fib_binet_res []) (set! fib_binet_i 0) (while (<= fib_binet_i fib_binet_n) (do (set! fib_binet_val (roundf (quot (powf fib_binet_phi fib_binet_i) fib_binet_sqrt5))) (set! fib_binet_res (conj fib_binet_res fib_binet_val)) (set! fib_binet_i (+ fib_binet_i 1)))) (throw (ex-info "return" {:v fib_binet_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_mul [matrix_mul_a matrix_mul_b]
  (binding [matrix_mul_a00 nil matrix_mul_a01 nil matrix_mul_a10 nil matrix_mul_a11 nil] (try (do (set! matrix_mul_a00 (+ (* (nth (nth matrix_mul_a 0) 0) (nth (nth matrix_mul_b 0) 0)) (* (nth (nth matrix_mul_a 0) 1) (nth (nth matrix_mul_b 1) 0)))) (set! matrix_mul_a01 (+ (* (nth (nth matrix_mul_a 0) 0) (nth (nth matrix_mul_b 0) 1)) (* (nth (nth matrix_mul_a 0) 1) (nth (nth matrix_mul_b 1) 1)))) (set! matrix_mul_a10 (+ (* (nth (nth matrix_mul_a 1) 0) (nth (nth matrix_mul_b 0) 0)) (* (nth (nth matrix_mul_a 1) 1) (nth (nth matrix_mul_b 1) 0)))) (set! matrix_mul_a11 (+ (* (nth (nth matrix_mul_a 1) 0) (nth (nth matrix_mul_b 0) 1)) (* (nth (nth matrix_mul_a 1) 1) (nth (nth matrix_mul_b 1) 1)))) (throw (ex-info "return" {:v [[matrix_mul_a00 matrix_mul_a01] [matrix_mul_a10 matrix_mul_a11]]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_pow [matrix_pow_m matrix_pow_power]
  (binding [matrix_pow_base nil matrix_pow_p nil matrix_pow_result nil] (try (do (when (< matrix_pow_power 0) (throw (Exception. "power is negative"))) (set! matrix_pow_result [[1 0] [0 1]]) (set! matrix_pow_base matrix_pow_m) (set! matrix_pow_p matrix_pow_power) (while (> matrix_pow_p 0) (do (when (= (mod matrix_pow_p 2) 1) (set! matrix_pow_result (matrix_mul matrix_pow_result matrix_pow_base))) (set! matrix_pow_base (matrix_mul matrix_pow_base matrix_pow_base)) (set! matrix_pow_p (long (quot matrix_pow_p 2))))) (throw (ex-info "return" {:v matrix_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fib_matrix [fib_matrix_n]
  (binding [fib_matrix_m nil fib_matrix_res nil] (try (do (when (< fib_matrix_n 0) (throw (Exception. "n is negative"))) (when (= fib_matrix_n 0) (throw (ex-info "return" {:v 0}))) (set! fib_matrix_m [[1 1] [1 0]]) (set! fib_matrix_res (matrix_pow fib_matrix_m (- fib_matrix_n 1))) (throw (ex-info "return" {:v (nth (nth fib_matrix_res 0) 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn run_tests []
  (binding [run_tests_bin nil run_tests_cache nil run_tests_expected nil run_tests_it nil run_tests_m nil run_tests_memo nil run_tests_rec nil] (try (do (set! run_tests_expected [0 1 1 2 3 5 8 13 21 34 55]) (set! run_tests_it (fib_iterative 10)) (set! run_tests_rec (fib_recursive 10)) (set! run_tests_cache (fib_recursive_cached 10)) (set! run_tests_memo (fib_memoization 10)) (set! run_tests_bin (fib_binet 10)) (set! run_tests_m (fib_matrix 10)) (when (not= run_tests_it run_tests_expected) (throw (Exception. "iterative failed"))) (when (not= run_tests_rec run_tests_expected) (throw (Exception. "recursive failed"))) (when (not= run_tests_cache run_tests_expected) (throw (Exception. "cached failed"))) (when (not= run_tests_memo run_tests_expected) (throw (Exception. "memoization failed"))) (when (not= run_tests_bin run_tests_expected) (throw (Exception. "binet failed"))) (when (not= run_tests_m 55) (throw (Exception. "matrix failed"))) (throw (ex-info "return" {:v run_tests_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (run_tests)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
