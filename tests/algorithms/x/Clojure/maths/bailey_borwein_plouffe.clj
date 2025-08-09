(ns main (:refer-clojure :exclude [mod_pow pow_float hex_digit floor_float subsum bailey_borwein_plouffe]))

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

(declare mod_pow pow_float hex_digit floor_float subsum bailey_borwein_plouffe)

(def ^:dynamic bailey_borwein_plouffe_digit nil)

(def ^:dynamic bailey_borwein_plouffe_fraction nil)

(def ^:dynamic bailey_borwein_plouffe_hd nil)

(def ^:dynamic bailey_borwein_plouffe_sum_result nil)

(def ^:dynamic floor_float_i nil)

(def ^:dynamic hex_digit_letters nil)

(def ^:dynamic main_digits nil)

(def ^:dynamic main_i nil)

(def ^:dynamic mod_pow_b nil)

(def ^:dynamic mod_pow_e nil)

(def ^:dynamic mod_pow_result nil)

(def ^:dynamic pow_float_exp nil)

(def ^:dynamic pow_float_i nil)

(def ^:dynamic pow_float_result nil)

(def ^:dynamic subsum_denominator nil)

(def ^:dynamic subsum_exponent nil)

(def ^:dynamic subsum_exponential_term nil)

(def ^:dynamic subsum_sum_index nil)

(def ^:dynamic subsum_total nil)

(defn mod_pow [mod_pow_base mod_pow_exponent mod_pow_modulus]
  (binding [mod_pow_b nil mod_pow_e nil mod_pow_result nil] (try (do (set! mod_pow_result 1) (set! mod_pow_b (mod mod_pow_base mod_pow_modulus)) (set! mod_pow_e mod_pow_exponent) (while (> mod_pow_e 0) (do (when (= (mod mod_pow_e 2) 1) (set! mod_pow_result (mod (* mod_pow_result mod_pow_b) mod_pow_modulus))) (set! mod_pow_b (mod (* mod_pow_b mod_pow_b) mod_pow_modulus)) (set! mod_pow_e (quot mod_pow_e 2)))) (throw (ex-info "return" {:v mod_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow_float [pow_float_base pow_float_exponent]
  (binding [pow_float_exp nil pow_float_i nil pow_float_result nil] (try (do (set! pow_float_exp pow_float_exponent) (set! pow_float_result 1.0) (when (< pow_float_exp 0) (set! pow_float_exp (- pow_float_exp))) (set! pow_float_i 0) (while (< pow_float_i pow_float_exp) (do (set! pow_float_result (* pow_float_result pow_float_base)) (set! pow_float_i (+ pow_float_i 1)))) (when (< pow_float_exponent 0) (set! pow_float_result (/ 1.0 pow_float_result))) (throw (ex-info "return" {:v pow_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hex_digit [hex_digit_n]
  (binding [hex_digit_letters nil] (try (do (when (< hex_digit_n 10) (throw (ex-info "return" {:v (str hex_digit_n)}))) (set! hex_digit_letters ["a" "b" "c" "d" "e" "f"]) (throw (ex-info "return" {:v (nth hex_digit_letters (- hex_digit_n 10))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn floor_float [floor_float_x]
  (binding [floor_float_i nil] (try (do (set! floor_float_i (long floor_float_x)) (when (> (double floor_float_i) floor_float_x) (set! floor_float_i (- floor_float_i 1))) (throw (ex-info "return" {:v (double floor_float_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn subsum [subsum_digit_pos_to_extract subsum_denominator_addend subsum_precision]
  (binding [subsum_denominator nil subsum_exponent nil subsum_exponential_term nil subsum_sum_index nil subsum_total nil] (try (do (set! subsum_total 0.0) (set! subsum_sum_index 0) (while (< subsum_sum_index (+ subsum_digit_pos_to_extract subsum_precision)) (do (set! subsum_denominator (+ (* 8 subsum_sum_index) subsum_denominator_addend)) (if (< subsum_sum_index subsum_digit_pos_to_extract) (do (set! subsum_exponent (- (- subsum_digit_pos_to_extract 1) subsum_sum_index)) (set! subsum_exponential_term (mod_pow 16 subsum_exponent subsum_denominator)) (set! subsum_total (+ subsum_total (quot (double subsum_exponential_term) (double subsum_denominator))))) (do (set! subsum_exponent (- (- subsum_digit_pos_to_extract 1) subsum_sum_index)) (set! subsum_exponential_term (pow_float 16.0 subsum_exponent)) (set! subsum_total (+ subsum_total (quot subsum_exponential_term (double subsum_denominator)))))) (set! subsum_sum_index (+ subsum_sum_index 1)))) (throw (ex-info "return" {:v subsum_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bailey_borwein_plouffe [bailey_borwein_plouffe_digit_position bailey_borwein_plouffe_precision]
  (binding [bailey_borwein_plouffe_digit nil bailey_borwein_plouffe_fraction nil bailey_borwein_plouffe_hd nil bailey_borwein_plouffe_sum_result nil] (try (do (when (<= bailey_borwein_plouffe_digit_position 0) (throw (Exception. "Digit position must be a positive integer"))) (when (< bailey_borwein_plouffe_precision 0) (throw (Exception. "Precision must be a nonnegative integer"))) (set! bailey_borwein_plouffe_sum_result (- (- (- (* 4.0 (subsum bailey_borwein_plouffe_digit_position 1 bailey_borwein_plouffe_precision)) (* 2.0 (subsum bailey_borwein_plouffe_digit_position 4 bailey_borwein_plouffe_precision))) (* 1.0 (subsum bailey_borwein_plouffe_digit_position 5 bailey_borwein_plouffe_precision))) (* 1.0 (subsum bailey_borwein_plouffe_digit_position 6 bailey_borwein_plouffe_precision)))) (set! bailey_borwein_plouffe_fraction (- bailey_borwein_plouffe_sum_result (floor_float bailey_borwein_plouffe_sum_result))) (set! bailey_borwein_plouffe_digit (long (* bailey_borwein_plouffe_fraction 16.0))) (set! bailey_borwein_plouffe_hd (hex_digit bailey_borwein_plouffe_digit)) (throw (ex-info "return" {:v bailey_borwein_plouffe_hd}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_digits "")

(def ^:dynamic main_i 1)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (<= main_i 10) (do (def main_digits (str main_digits (bailey_borwein_plouffe main_i 1000))) (def main_i (+ main_i 1))))
      (println main_digits)
      (println (bailey_borwein_plouffe 5 10000))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
