(ns main (:refer-clojure :exclude [pow2_int pow2_float lshift rshift log2_floor float_to_bits bits_to_float absf sqrtApprox is_close fast_inverse_sqrt test_fast_inverse_sqrt main]))

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

(declare pow2_int pow2_float lshift rshift log2_floor float_to_bits bits_to_float absf sqrtApprox is_close fast_inverse_sqrt test_fast_inverse_sqrt main)

(def ^:dynamic bits_to_float_exp nil)

(def ^:dynamic bits_to_float_exp_bits nil)

(def ^:dynamic bits_to_float_mantissa nil)

(def ^:dynamic bits_to_float_mantissa_bits nil)

(def ^:dynamic bits_to_float_sign nil)

(def ^:dynamic bits_to_float_sign_bit nil)

(def ^:dynamic fast_inverse_sqrt_i nil)

(def ^:dynamic fast_inverse_sqrt_magic nil)

(def ^:dynamic fast_inverse_sqrt_y nil)

(def ^:dynamic fast_inverse_sqrt_y_bits nil)

(def ^:dynamic float_to_bits_exp nil)

(def ^:dynamic float_to_bits_exp_bits nil)

(def ^:dynamic float_to_bits_frac nil)

(def ^:dynamic float_to_bits_mantissa nil)

(def ^:dynamic float_to_bits_normalized nil)

(def ^:dynamic float_to_bits_num nil)

(def ^:dynamic float_to_bits_pow nil)

(def ^:dynamic float_to_bits_sign nil)

(def ^:dynamic log2_floor_e nil)

(def ^:dynamic log2_floor_n nil)

(def ^:dynamic lshift_i nil)

(def ^:dynamic lshift_result nil)

(def ^:dynamic main_diff nil)

(def ^:dynamic main_i nil)

(def ^:dynamic pow2_float_i nil)

(def ^:dynamic pow2_float_m nil)

(def ^:dynamic pow2_float_result nil)

(def ^:dynamic pow2_int_i nil)

(def ^:dynamic pow2_int_result nil)

(def ^:dynamic rshift_i nil)

(def ^:dynamic rshift_result nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic test_fast_inverse_sqrt_actual nil)

(def ^:dynamic test_fast_inverse_sqrt_i nil)

(def ^:dynamic test_fast_inverse_sqrt_y nil)

(defn pow2_int [pow2_int_n]
  (binding [pow2_int_i nil pow2_int_result nil] (try (do (set! pow2_int_result 1) (set! pow2_int_i 0) (while (< pow2_int_i pow2_int_n) (do (set! pow2_int_result (* pow2_int_result 2)) (set! pow2_int_i (+ pow2_int_i 1)))) (throw (ex-info "return" {:v pow2_int_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow2_float [pow2_float_n]
  (binding [pow2_float_i nil pow2_float_m nil pow2_float_result nil] (try (do (set! pow2_float_result 1.0) (if (>= pow2_float_n 0) (do (set! pow2_float_i 0) (while (< pow2_float_i pow2_float_n) (do (set! pow2_float_result (* pow2_float_result 2.0)) (set! pow2_float_i (+ pow2_float_i 1))))) (do (set! pow2_float_i 0) (set! pow2_float_m (- 0 pow2_float_n)) (while (< pow2_float_i pow2_float_m) (do (set! pow2_float_result (/ pow2_float_result 2.0)) (set! pow2_float_i (+ pow2_float_i 1)))))) (throw (ex-info "return" {:v pow2_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lshift [lshift_num lshift_k]
  (binding [lshift_i nil lshift_result nil] (try (do (set! lshift_result lshift_num) (set! lshift_i 0) (while (< lshift_i lshift_k) (do (set! lshift_result (* lshift_result 2)) (set! lshift_i (+ lshift_i 1)))) (throw (ex-info "return" {:v lshift_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rshift [rshift_num rshift_k]
  (binding [rshift_i nil rshift_result nil] (try (do (set! rshift_result rshift_num) (set! rshift_i 0) (while (< rshift_i rshift_k) (do (set! rshift_result (quot (- rshift_result (mod rshift_result 2)) 2)) (set! rshift_i (+ rshift_i 1)))) (throw (ex-info "return" {:v rshift_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn log2_floor [log2_floor_x]
  (binding [log2_floor_e nil log2_floor_n nil] (try (do (set! log2_floor_n log2_floor_x) (set! log2_floor_e 0) (while (>= log2_floor_n 2.0) (do (set! log2_floor_n (/ log2_floor_n 2.0)) (set! log2_floor_e (+ log2_floor_e 1)))) (while (< log2_floor_n 1.0) (do (set! log2_floor_n (* log2_floor_n 2.0)) (set! log2_floor_e (- log2_floor_e 1)))) (throw (ex-info "return" {:v log2_floor_e}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn float_to_bits [float_to_bits_x]
  (binding [float_to_bits_exp nil float_to_bits_exp_bits nil float_to_bits_frac nil float_to_bits_mantissa nil float_to_bits_normalized nil float_to_bits_num nil float_to_bits_pow nil float_to_bits_sign nil] (try (do (set! float_to_bits_num float_to_bits_x) (set! float_to_bits_sign 0) (when (< float_to_bits_num 0.0) (do (set! float_to_bits_sign 1) (set! float_to_bits_num (- float_to_bits_num)))) (set! float_to_bits_exp (log2_floor float_to_bits_num)) (set! float_to_bits_pow (pow2_float float_to_bits_exp)) (set! float_to_bits_normalized (quot float_to_bits_num float_to_bits_pow)) (set! float_to_bits_frac (- float_to_bits_normalized 1.0)) (set! float_to_bits_mantissa (long (* float_to_bits_frac (pow2_float 23)))) (set! float_to_bits_exp_bits (+ float_to_bits_exp 127)) (throw (ex-info "return" {:v (+ (+ (lshift float_to_bits_sign 31) (lshift float_to_bits_exp_bits 23)) float_to_bits_mantissa)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bits_to_float [bits_to_float_bits]
  (binding [bits_to_float_exp nil bits_to_float_exp_bits nil bits_to_float_mantissa nil bits_to_float_mantissa_bits nil bits_to_float_sign nil bits_to_float_sign_bit nil] (try (do (set! bits_to_float_sign_bit (mod (rshift bits_to_float_bits 31) 2)) (set! bits_to_float_sign 1.0) (when (= bits_to_float_sign_bit 1) (set! bits_to_float_sign (- 1.0))) (set! bits_to_float_exp_bits (mod (rshift bits_to_float_bits 23) 256)) (set! bits_to_float_exp (- bits_to_float_exp_bits 127)) (set! bits_to_float_mantissa_bits (mod bits_to_float_bits (pow2_int 23))) (set! bits_to_float_mantissa (+ 1.0 (quot (double bits_to_float_mantissa_bits) (pow2_float 23)))) (throw (ex-info "return" {:v (* (* bits_to_float_sign bits_to_float_mantissa) (pow2_float bits_to_float_exp))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_close [is_close_a is_close_b is_close_rel_tol]
  (try (throw (ex-info "return" {:v (<= (absf (- is_close_a is_close_b)) (* is_close_rel_tol (absf is_close_b)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fast_inverse_sqrt [fast_inverse_sqrt_number]
  (binding [fast_inverse_sqrt_i nil fast_inverse_sqrt_magic nil fast_inverse_sqrt_y nil fast_inverse_sqrt_y_bits nil] (try (do (when (<= fast_inverse_sqrt_number 0.0) (throw (Exception. "Input must be a positive number."))) (set! fast_inverse_sqrt_i (float_to_bits fast_inverse_sqrt_number)) (set! fast_inverse_sqrt_magic 1597463007) (set! fast_inverse_sqrt_y_bits (- fast_inverse_sqrt_magic (rshift fast_inverse_sqrt_i 1))) (set! fast_inverse_sqrt_y (bits_to_float fast_inverse_sqrt_y_bits)) (set! fast_inverse_sqrt_y (* fast_inverse_sqrt_y (- 1.5 (* (* (* 0.5 fast_inverse_sqrt_number) fast_inverse_sqrt_y) fast_inverse_sqrt_y)))) (throw (ex-info "return" {:v fast_inverse_sqrt_y}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_fast_inverse_sqrt []
  (binding [test_fast_inverse_sqrt_actual nil test_fast_inverse_sqrt_i nil test_fast_inverse_sqrt_y nil] (do (when (> (absf (- (fast_inverse_sqrt 10.0) 0.3156857923527257)) 0.0001) (throw (Exception. "fast_inverse_sqrt(10) failed"))) (when (> (absf (- (fast_inverse_sqrt 4.0) 0.49915357479239103)) 0.0001) (throw (Exception. "fast_inverse_sqrt(4) failed"))) (when (> (absf (- (fast_inverse_sqrt 4.1) 0.4932849504615651)) 0.0001) (throw (Exception. "fast_inverse_sqrt(4.1) failed"))) (set! test_fast_inverse_sqrt_i 50) (while (< test_fast_inverse_sqrt_i 60) (do (set! test_fast_inverse_sqrt_y (fast_inverse_sqrt (double test_fast_inverse_sqrt_i))) (set! test_fast_inverse_sqrt_actual (/ 1.0 (sqrtApprox (double test_fast_inverse_sqrt_i)))) (when (not (is_close test_fast_inverse_sqrt_y test_fast_inverse_sqrt_actual 0.00132)) (throw (Exception. "relative error too high"))) (set! test_fast_inverse_sqrt_i (+ test_fast_inverse_sqrt_i 1)))))))

(defn main []
  (binding [main_diff nil main_i nil] (do (test_fast_inverse_sqrt) (set! main_i 5) (while (<= main_i 100) (do (set! main_diff (- (/ 1.0 (sqrtApprox (double main_i))) (fast_inverse_sqrt (double main_i)))) (println (str (str (str main_i) ": ") (str main_diff))) (set! main_i (+ main_i 5)))))))

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
