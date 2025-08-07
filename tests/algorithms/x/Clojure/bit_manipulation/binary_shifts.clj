(ns main (:refer-clojure :exclude [repeat_char abs_int pow2 to_binary_no_prefix logical_left_shift logical_right_shift arithmetic_right_shift main]))

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

(declare repeat_char abs_int pow2 to_binary_no_prefix logical_left_shift logical_right_shift arithmetic_right_shift main)

(def ^:dynamic arithmetic_right_shift_bin_repr nil)

(def ^:dynamic arithmetic_right_shift_binary_number nil)

(def ^:dynamic arithmetic_right_shift_intermediate nil)

(def ^:dynamic arithmetic_right_shift_length nil)

(def ^:dynamic arithmetic_right_shift_shifted nil)

(def ^:dynamic arithmetic_right_shift_sign nil)

(def ^:dynamic logical_left_shift_binary_number nil)

(def ^:dynamic logical_right_shift_binary_number nil)

(def ^:dynamic logical_right_shift_shifted nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_res nil)

(def ^:dynamic repeat_char_i nil)

(def ^:dynamic repeat_char_res nil)

(def ^:dynamic to_binary_no_prefix_res nil)

(def ^:dynamic to_binary_no_prefix_v nil)

(defn repeat_char [repeat_char_ch count_v]
  (binding [repeat_char_i nil repeat_char_res nil] (try (do (set! repeat_char_res "") (set! repeat_char_i 0) (while (< repeat_char_i count_v) (do (set! repeat_char_res (str repeat_char_res repeat_char_ch)) (set! repeat_char_i (+ repeat_char_i 1)))) (throw (ex-info "return" {:v repeat_char_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs_int [abs_int_n]
  (try (if (< abs_int_n 0) (- abs_int_n) abs_int_n) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pow2 [pow2_exp]
  (binding [pow2_i nil pow2_res nil] (try (do (set! pow2_res 1) (set! pow2_i 0) (while (< pow2_i pow2_exp) (do (set! pow2_res (* pow2_res 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_binary_no_prefix [to_binary_no_prefix_n]
  (binding [to_binary_no_prefix_res nil to_binary_no_prefix_v nil] (try (do (set! to_binary_no_prefix_v to_binary_no_prefix_n) (when (< to_binary_no_prefix_v 0) (set! to_binary_no_prefix_v (- to_binary_no_prefix_v))) (when (= to_binary_no_prefix_v 0) (throw (ex-info "return" {:v "0"}))) (set! to_binary_no_prefix_res "") (while (> to_binary_no_prefix_v 0) (do (set! to_binary_no_prefix_res (str (str (mod to_binary_no_prefix_v 2)) to_binary_no_prefix_res)) (set! to_binary_no_prefix_v (quot to_binary_no_prefix_v 2)))) (throw (ex-info "return" {:v to_binary_no_prefix_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn logical_left_shift [logical_left_shift_number logical_left_shift_shift_amount]
  (binding [logical_left_shift_binary_number nil] (try (do (when (or (< logical_left_shift_number 0) (< logical_left_shift_shift_amount 0)) (throw (Exception. "both inputs must be positive integers"))) (set! logical_left_shift_binary_number (str "0b" (to_binary_no_prefix logical_left_shift_number))) (throw (ex-info "return" {:v (str logical_left_shift_binary_number (repeat_char "0" logical_left_shift_shift_amount))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn logical_right_shift [logical_right_shift_number logical_right_shift_shift_amount]
  (binding [logical_right_shift_binary_number nil logical_right_shift_shifted nil] (try (do (when (or (< logical_right_shift_number 0) (< logical_right_shift_shift_amount 0)) (throw (Exception. "both inputs must be positive integers"))) (set! logical_right_shift_binary_number (to_binary_no_prefix logical_right_shift_number)) (when (>= logical_right_shift_shift_amount (count logical_right_shift_binary_number)) (throw (ex-info "return" {:v "0b0"}))) (set! logical_right_shift_shifted (subs logical_right_shift_binary_number 0 (min (- (count logical_right_shift_binary_number) logical_right_shift_shift_amount) (count logical_right_shift_binary_number)))) (throw (ex-info "return" {:v (str "0b" logical_right_shift_shifted)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn arithmetic_right_shift [arithmetic_right_shift_number arithmetic_right_shift_shift_amount]
  (binding [arithmetic_right_shift_bin_repr nil arithmetic_right_shift_binary_number nil arithmetic_right_shift_intermediate nil arithmetic_right_shift_length nil arithmetic_right_shift_shifted nil arithmetic_right_shift_sign nil] (try (do (set! arithmetic_right_shift_binary_number "") (if (>= arithmetic_right_shift_number 0) (set! arithmetic_right_shift_binary_number (str "0" (to_binary_no_prefix arithmetic_right_shift_number))) (do (set! arithmetic_right_shift_length (count (to_binary_no_prefix (- arithmetic_right_shift_number)))) (set! arithmetic_right_shift_intermediate (- (abs_int arithmetic_right_shift_number) (pow2 arithmetic_right_shift_length))) (set! arithmetic_right_shift_bin_repr (to_binary_no_prefix arithmetic_right_shift_intermediate)) (set! arithmetic_right_shift_binary_number (str (str "1" (repeat_char "0" (- arithmetic_right_shift_length (count arithmetic_right_shift_bin_repr)))) arithmetic_right_shift_bin_repr)))) (when (>= arithmetic_right_shift_shift_amount (count arithmetic_right_shift_binary_number)) (do (set! arithmetic_right_shift_sign (subs arithmetic_right_shift_binary_number 0 (min 1 (count arithmetic_right_shift_binary_number)))) (throw (ex-info "return" {:v (str "0b" (repeat_char arithmetic_right_shift_sign (count arithmetic_right_shift_binary_number)))})))) (set! arithmetic_right_shift_sign (subs arithmetic_right_shift_binary_number 0 (min 1 (count arithmetic_right_shift_binary_number)))) (set! arithmetic_right_shift_shifted (subs arithmetic_right_shift_binary_number 0 (min (- (count arithmetic_right_shift_binary_number) arithmetic_right_shift_shift_amount) (count arithmetic_right_shift_binary_number)))) (throw (ex-info "return" {:v (str (str "0b" (repeat_char arithmetic_right_shift_sign arithmetic_right_shift_shift_amount)) arithmetic_right_shift_shifted)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (logical_left_shift 17 2)) (println (logical_right_shift 1983 4)) (println (arithmetic_right_shift (- 17) 2))))

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
