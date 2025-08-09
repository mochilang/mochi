(ns main (:refer-clojure :exclude [panic char_to_value int_to_base base_to_int sum_of_digits harshad_numbers_in_base is_harshad_number_in_base main]))

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

(declare panic char_to_value int_to_base base_to_int sum_of_digits harshad_numbers_in_base is_harshad_number_in_base main)

(def ^:dynamic base_to_int_c nil)

(def ^:dynamic base_to_int_i nil)

(def ^:dynamic base_to_int_value nil)

(def ^:dynamic char_to_value_digits nil)

(def ^:dynamic char_to_value_i nil)

(def ^:dynamic harshad_numbers_in_base_divisor nil)

(def ^:dynamic harshad_numbers_in_base_i nil)

(def ^:dynamic harshad_numbers_in_base_numbers nil)

(def ^:dynamic harshad_numbers_in_base_s nil)

(def ^:dynamic int_to_base_digits nil)

(def ^:dynamic int_to_base_n nil)

(def ^:dynamic int_to_base_remainder nil)

(def ^:dynamic int_to_base_result nil)

(def ^:dynamic is_harshad_number_in_base_d nil)

(def ^:dynamic is_harshad_number_in_base_d_val nil)

(def ^:dynamic is_harshad_number_in_base_n nil)

(def ^:dynamic is_harshad_number_in_base_n_val nil)

(def ^:dynamic sum_of_digits_c nil)

(def ^:dynamic sum_of_digits_i nil)

(def ^:dynamic sum_of_digits_num_str nil)

(def ^:dynamic sum_of_digits_total nil)

(defn panic [panic_msg]
  (do))

(defn char_to_value [char_to_value_c]
  (binding [char_to_value_digits nil char_to_value_i nil] (try (do (set! char_to_value_digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! char_to_value_i 0) (while (< char_to_value_i (count char_to_value_digits)) (do (when (= (subs char_to_value_digits char_to_value_i (+ char_to_value_i 1)) char_to_value_c) (throw (ex-info "return" {:v char_to_value_i}))) (set! char_to_value_i (+ char_to_value_i 1)))) (panic "invalid digit")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn int_to_base [int_to_base_number int_to_base_base]
  (binding [int_to_base_digits nil int_to_base_n nil int_to_base_remainder nil int_to_base_result nil] (try (do (when (or (< int_to_base_base 2) (> int_to_base_base 36)) (panic "'base' must be between 2 and 36 inclusive")) (when (< int_to_base_number 0) (panic "number must be a positive integer")) (set! int_to_base_digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! int_to_base_n int_to_base_number) (set! int_to_base_result "") (while (> int_to_base_n 0) (do (set! int_to_base_remainder (mod int_to_base_n int_to_base_base)) (set! int_to_base_result (str (subs int_to_base_digits int_to_base_remainder (+ int_to_base_remainder 1)) int_to_base_result)) (set! int_to_base_n (quot int_to_base_n int_to_base_base)))) (when (= int_to_base_result "") (set! int_to_base_result "0")) (throw (ex-info "return" {:v int_to_base_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn base_to_int [base_to_int_num_str base_to_int_base]
  (binding [base_to_int_c nil base_to_int_i nil base_to_int_value nil] (try (do (set! base_to_int_value 0) (set! base_to_int_i 0) (while (< base_to_int_i (count base_to_int_num_str)) (do (set! base_to_int_c (subs base_to_int_num_str base_to_int_i (+ base_to_int_i 1))) (set! base_to_int_value (+ (* base_to_int_value base_to_int_base) (char_to_value base_to_int_c))) (set! base_to_int_i (+ base_to_int_i 1)))) (throw (ex-info "return" {:v base_to_int_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sum_of_digits [sum_of_digits_num sum_of_digits_base]
  (binding [sum_of_digits_c nil sum_of_digits_i nil sum_of_digits_num_str nil sum_of_digits_total nil] (try (do (when (or (< sum_of_digits_base 2) (> sum_of_digits_base 36)) (panic "'base' must be between 2 and 36 inclusive")) (set! sum_of_digits_num_str (int_to_base sum_of_digits_num sum_of_digits_base)) (set! sum_of_digits_total 0) (set! sum_of_digits_i 0) (while (< sum_of_digits_i (count sum_of_digits_num_str)) (do (set! sum_of_digits_c (subs sum_of_digits_num_str sum_of_digits_i (+ sum_of_digits_i 1))) (set! sum_of_digits_total (+ sum_of_digits_total (char_to_value sum_of_digits_c))) (set! sum_of_digits_i (+ sum_of_digits_i 1)))) (throw (ex-info "return" {:v (int_to_base sum_of_digits_total sum_of_digits_base)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn harshad_numbers_in_base [harshad_numbers_in_base_limit harshad_numbers_in_base_base]
  (binding [harshad_numbers_in_base_divisor nil harshad_numbers_in_base_i nil harshad_numbers_in_base_numbers nil harshad_numbers_in_base_s nil] (try (do (when (or (< harshad_numbers_in_base_base 2) (> harshad_numbers_in_base_base 36)) (panic "'base' must be between 2 and 36 inclusive")) (when (< harshad_numbers_in_base_limit 0) (throw (ex-info "return" {:v []}))) (set! harshad_numbers_in_base_numbers []) (set! harshad_numbers_in_base_i 1) (while (< harshad_numbers_in_base_i harshad_numbers_in_base_limit) (do (set! harshad_numbers_in_base_s (sum_of_digits harshad_numbers_in_base_i harshad_numbers_in_base_base)) (set! harshad_numbers_in_base_divisor (base_to_int harshad_numbers_in_base_s harshad_numbers_in_base_base)) (when (= (mod harshad_numbers_in_base_i harshad_numbers_in_base_divisor) 0) (set! harshad_numbers_in_base_numbers (conj harshad_numbers_in_base_numbers (int_to_base harshad_numbers_in_base_i harshad_numbers_in_base_base)))) (set! harshad_numbers_in_base_i (+ harshad_numbers_in_base_i 1)))) (throw (ex-info "return" {:v harshad_numbers_in_base_numbers}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_harshad_number_in_base [is_harshad_number_in_base_num is_harshad_number_in_base_base]
  (binding [is_harshad_number_in_base_d nil is_harshad_number_in_base_d_val nil is_harshad_number_in_base_n nil is_harshad_number_in_base_n_val nil] (try (do (when (or (< is_harshad_number_in_base_base 2) (> is_harshad_number_in_base_base 36)) (panic "'base' must be between 2 and 36 inclusive")) (when (< is_harshad_number_in_base_num 0) (throw (ex-info "return" {:v false}))) (set! is_harshad_number_in_base_n (int_to_base is_harshad_number_in_base_num is_harshad_number_in_base_base)) (set! is_harshad_number_in_base_d (sum_of_digits is_harshad_number_in_base_num is_harshad_number_in_base_base)) (set! is_harshad_number_in_base_n_val (base_to_int is_harshad_number_in_base_n is_harshad_number_in_base_base)) (set! is_harshad_number_in_base_d_val (base_to_int is_harshad_number_in_base_d is_harshad_number_in_base_base)) (throw (ex-info "return" {:v (= (mod is_harshad_number_in_base_n_val is_harshad_number_in_base_d_val) 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (int_to_base 0 21)) (println (int_to_base 23 2)) (println (int_to_base 58 5)) (println (int_to_base 167 16)) (println (sum_of_digits 103 12)) (println (sum_of_digits 1275 4)) (println (sum_of_digits 6645 2)) (println (harshad_numbers_in_base 15 2)) (println (harshad_numbers_in_base 12 34)) (println (harshad_numbers_in_base 12 4)) (println (is_harshad_number_in_base 18 10)) (println (is_harshad_number_in_base 21 10)) (println (is_harshad_number_in_base (- 21) 5))))

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
