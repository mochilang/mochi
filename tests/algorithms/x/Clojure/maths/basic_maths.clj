(ns main (:refer-clojure :exclude [pow_int prime_factors number_of_divisors sum_of_divisors contains unique euler_phi]))

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

(declare pow_int prime_factors number_of_divisors sum_of_divisors contains unique euler_phi)

(def ^:dynamic contains_idx nil)

(def ^:dynamic euler_phi_factors nil)

(def ^:dynamic euler_phi_idx nil)

(def ^:dynamic euler_phi_s nil)

(def ^:dynamic euler_phi_x nil)

(def ^:dynamic number_of_divisors_div nil)

(def ^:dynamic number_of_divisors_i nil)

(def ^:dynamic number_of_divisors_num nil)

(def ^:dynamic number_of_divisors_temp nil)

(def ^:dynamic pow_int_i nil)

(def ^:dynamic pow_int_result nil)

(def ^:dynamic prime_factors_i nil)

(def ^:dynamic prime_factors_num nil)

(def ^:dynamic prime_factors_pf nil)

(def ^:dynamic sum_of_divisors_i nil)

(def ^:dynamic sum_of_divisors_num nil)

(def ^:dynamic sum_of_divisors_s nil)

(def ^:dynamic sum_of_divisors_temp nil)

(def ^:dynamic unique_idx nil)

(def ^:dynamic unique_result nil)

(def ^:dynamic unique_v nil)

(defn pow_int [pow_int_base pow_int_exp]
  (binding [pow_int_i nil pow_int_result nil] (try (do (set! pow_int_result 1) (set! pow_int_i 0) (while (< pow_int_i pow_int_exp) (do (set! pow_int_result (* pow_int_result pow_int_base)) (set! pow_int_i (+ pow_int_i 1)))) (throw (ex-info "return" {:v pow_int_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn prime_factors [prime_factors_n]
  (binding [prime_factors_i nil prime_factors_num nil prime_factors_pf nil] (try (do (when (<= prime_factors_n 0) (throw (Exception. "Only positive integers have prime factors"))) (set! prime_factors_num prime_factors_n) (set! prime_factors_pf []) (while (= (mod prime_factors_num 2) 0) (do (set! prime_factors_pf (conj prime_factors_pf 2)) (set! prime_factors_num (quot prime_factors_num 2)))) (set! prime_factors_i 3) (while (<= (* prime_factors_i prime_factors_i) prime_factors_num) (do (while (= (mod prime_factors_num prime_factors_i) 0) (do (set! prime_factors_pf (conj prime_factors_pf prime_factors_i)) (set! prime_factors_num (quot prime_factors_num prime_factors_i)))) (set! prime_factors_i (+ prime_factors_i 2)))) (when (> prime_factors_num 2) (set! prime_factors_pf (conj prime_factors_pf prime_factors_num))) (throw (ex-info "return" {:v prime_factors_pf}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn number_of_divisors [number_of_divisors_n]
  (binding [number_of_divisors_div nil number_of_divisors_i nil number_of_divisors_num nil number_of_divisors_temp nil] (try (do (when (<= number_of_divisors_n 0) (throw (Exception. "Only positive numbers are accepted"))) (set! number_of_divisors_num number_of_divisors_n) (set! number_of_divisors_div 1) (set! number_of_divisors_temp 1) (while (= (mod number_of_divisors_num 2) 0) (do (set! number_of_divisors_temp (+ number_of_divisors_temp 1)) (set! number_of_divisors_num (quot number_of_divisors_num 2)))) (set! number_of_divisors_div (* number_of_divisors_div number_of_divisors_temp)) (set! number_of_divisors_i 3) (while (<= (* number_of_divisors_i number_of_divisors_i) number_of_divisors_num) (do (set! number_of_divisors_temp 1) (while (= (mod number_of_divisors_num number_of_divisors_i) 0) (do (set! number_of_divisors_temp (+ number_of_divisors_temp 1)) (set! number_of_divisors_num (quot number_of_divisors_num number_of_divisors_i)))) (set! number_of_divisors_div (* number_of_divisors_div number_of_divisors_temp)) (set! number_of_divisors_i (+ number_of_divisors_i 2)))) (when (> number_of_divisors_num 1) (set! number_of_divisors_div (* number_of_divisors_div 2))) (throw (ex-info "return" {:v number_of_divisors_div}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sum_of_divisors [sum_of_divisors_n]
  (binding [sum_of_divisors_i nil sum_of_divisors_num nil sum_of_divisors_s nil sum_of_divisors_temp nil] (try (do (when (<= sum_of_divisors_n 0) (throw (Exception. "Only positive numbers are accepted"))) (set! sum_of_divisors_num sum_of_divisors_n) (set! sum_of_divisors_s 1) (set! sum_of_divisors_temp 1) (while (= (mod sum_of_divisors_num 2) 0) (do (set! sum_of_divisors_temp (+ sum_of_divisors_temp 1)) (set! sum_of_divisors_num (quot sum_of_divisors_num 2)))) (when (> sum_of_divisors_temp 1) (set! sum_of_divisors_s (* sum_of_divisors_s (quot (- (pow_int 2 sum_of_divisors_temp) 1) (- 2 1))))) (set! sum_of_divisors_i 3) (while (<= (* sum_of_divisors_i sum_of_divisors_i) sum_of_divisors_num) (do (set! sum_of_divisors_temp 1) (while (= (mod sum_of_divisors_num sum_of_divisors_i) 0) (do (set! sum_of_divisors_temp (+ sum_of_divisors_temp 1)) (set! sum_of_divisors_num (quot sum_of_divisors_num sum_of_divisors_i)))) (when (> sum_of_divisors_temp 1) (set! sum_of_divisors_s (* sum_of_divisors_s (quot (- (pow_int sum_of_divisors_i sum_of_divisors_temp) 1) (- sum_of_divisors_i 1))))) (set! sum_of_divisors_i (+ sum_of_divisors_i 2)))) (throw (ex-info "return" {:v sum_of_divisors_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains [contains_arr contains_x]
  (binding [contains_idx nil] (try (do (set! contains_idx 0) (while (< contains_idx (count contains_arr)) (do (when (= (nth contains_arr contains_idx) contains_x) (throw (ex-info "return" {:v true}))) (set! contains_idx (+ contains_idx 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn unique [unique_arr]
  (binding [unique_idx nil unique_result nil unique_v nil] (try (do (set! unique_result []) (set! unique_idx 0) (while (< unique_idx (count unique_arr)) (do (set! unique_v (nth unique_arr unique_idx)) (when (not (contains unique_result unique_v)) (set! unique_result (conj unique_result unique_v))) (set! unique_idx (+ unique_idx 1)))) (throw (ex-info "return" {:v unique_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn euler_phi [euler_phi_n]
  (binding [euler_phi_factors nil euler_phi_idx nil euler_phi_s nil euler_phi_x nil] (try (do (when (<= euler_phi_n 0) (throw (Exception. "Only positive numbers are accepted"))) (set! euler_phi_s euler_phi_n) (set! euler_phi_factors (unique (prime_factors euler_phi_n))) (set! euler_phi_idx 0) (while (< euler_phi_idx (count euler_phi_factors)) (do (set! euler_phi_x (nth euler_phi_factors euler_phi_idx)) (set! euler_phi_s (* (quot euler_phi_s euler_phi_x) (- euler_phi_x 1))) (set! euler_phi_idx (+ euler_phi_idx 1)))) (throw (ex-info "return" {:v euler_phi_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (prime_factors 100)))
      (println (str (number_of_divisors 100)))
      (println (str (sum_of_divisors 100)))
      (println (str (euler_phi 100)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
