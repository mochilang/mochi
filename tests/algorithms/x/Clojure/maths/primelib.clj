(ns main (:refer-clojure :exclude [abs_int gcd_iter is_prime sieve_er get_prime_numbers prime_factorization greatest_prime_factor smallest_prime_factor kg_v is_even is_odd goldbach get_prime get_primes_between get_divisors is_perfect_number simplify_fraction factorial fib]))

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

(declare abs_int gcd_iter is_prime sieve_er get_prime_numbers prime_factorization greatest_prime_factor smallest_prime_factor kg_v is_even is_odd goldbach get_prime get_primes_between get_divisors is_perfect_number simplify_fraction factorial fib)

(def ^:dynamic factorial_ans nil)

(def ^:dynamic factorial_i nil)

(def ^:dynamic fib_ans nil)

(def ^:dynamic fib_fib1 nil)

(def ^:dynamic fib_i nil)

(def ^:dynamic fib_tmp nil)

(def ^:dynamic gcd_iter_t nil)

(def ^:dynamic gcd_iter_x nil)

(def ^:dynamic gcd_iter_y nil)

(def ^:dynamic get_divisors_ans nil)

(def ^:dynamic get_divisors_d nil)

(def ^:dynamic get_prime_ans nil)

(def ^:dynamic get_prime_index nil)

(def ^:dynamic get_prime_numbers_ans nil)

(def ^:dynamic get_prime_numbers_num nil)

(def ^:dynamic get_primes_between_ans nil)

(def ^:dynamic get_primes_between_bad1 nil)

(def ^:dynamic get_primes_between_bad2 nil)

(def ^:dynamic get_primes_between_num nil)

(def ^:dynamic goldbach_i nil)

(def ^:dynamic goldbach_j nil)

(def ^:dynamic goldbach_primes nil)

(def ^:dynamic greatest_prime_factor_factors nil)

(def ^:dynamic greatest_prime_factor_i nil)

(def ^:dynamic greatest_prime_factor_m nil)

(def ^:dynamic is_perfect_number_divisors nil)

(def ^:dynamic is_perfect_number_i nil)

(def ^:dynamic is_perfect_number_sum nil)

(def ^:dynamic is_prime_d nil)

(def ^:dynamic kg_v_g nil)

(def ^:dynamic prime_factorization_ans nil)

(def ^:dynamic prime_factorization_factor nil)

(def ^:dynamic prime_factorization_quotient nil)

(def ^:dynamic sieve_er_i nil)

(def ^:dynamic sieve_er_idx nil)

(def ^:dynamic sieve_er_j nil)

(def ^:dynamic sieve_er_k nil)

(def ^:dynamic sieve_er_nums nil)

(def ^:dynamic sieve_er_res nil)

(def ^:dynamic sieve_er_v nil)

(def ^:dynamic simplify_fraction_g nil)

(def ^:dynamic smallest_prime_factor_factors nil)

(def ^:dynamic smallest_prime_factor_i nil)

(def ^:dynamic smallest_prime_factor_m nil)

(defn abs_int [abs_int_x]
  (try (if (< abs_int_x 0) (- abs_int_x) abs_int_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn gcd_iter [gcd_iter_a gcd_iter_b]
  (binding [gcd_iter_t nil gcd_iter_x nil gcd_iter_y nil] (try (do (set! gcd_iter_x (abs_int gcd_iter_a)) (set! gcd_iter_y (abs_int gcd_iter_b)) (while (not= gcd_iter_y 0) (do (set! gcd_iter_t gcd_iter_y) (set! gcd_iter_y (mod gcd_iter_x gcd_iter_y)) (set! gcd_iter_x gcd_iter_t))) (throw (ex-info "return" {:v gcd_iter_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_prime [is_prime_n]
  (binding [is_prime_d nil] (try (do (when (<= is_prime_n 1) (throw (ex-info "return" {:v false}))) (set! is_prime_d 2) (while (<= (* is_prime_d is_prime_d) is_prime_n) (do (when (= (mod is_prime_n is_prime_d) 0) (throw (ex-info "return" {:v false}))) (set! is_prime_d (+ is_prime_d 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sieve_er [sieve_er_n]
  (binding [sieve_er_i nil sieve_er_idx nil sieve_er_j nil sieve_er_k nil sieve_er_nums nil sieve_er_res nil sieve_er_v nil] (try (do (set! sieve_er_nums []) (set! sieve_er_i 2) (while (<= sieve_er_i sieve_er_n) (do (set! sieve_er_nums (conj sieve_er_nums sieve_er_i)) (set! sieve_er_i (+ sieve_er_i 1)))) (set! sieve_er_idx 0) (while (< sieve_er_idx (count sieve_er_nums)) (do (set! sieve_er_j (+ sieve_er_idx 1)) (while (< sieve_er_j (count sieve_er_nums)) (do (when (not= (nth sieve_er_nums sieve_er_idx) 0) (when (= (mod (nth sieve_er_nums sieve_er_j) (nth sieve_er_nums sieve_er_idx)) 0) (set! sieve_er_nums (assoc sieve_er_nums sieve_er_j 0)))) (set! sieve_er_j (+ sieve_er_j 1)))) (set! sieve_er_idx (+ sieve_er_idx 1)))) (set! sieve_er_res []) (set! sieve_er_k 0) (while (< sieve_er_k (count sieve_er_nums)) (do (set! sieve_er_v (nth sieve_er_nums sieve_er_k)) (when (not= sieve_er_v 0) (set! sieve_er_res (conj sieve_er_res sieve_er_v))) (set! sieve_er_k (+ sieve_er_k 1)))) (throw (ex-info "return" {:v sieve_er_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_prime_numbers [get_prime_numbers_n]
  (binding [get_prime_numbers_ans nil get_prime_numbers_num nil] (try (do (set! get_prime_numbers_ans []) (set! get_prime_numbers_num 2) (while (<= get_prime_numbers_num get_prime_numbers_n) (do (when (is_prime get_prime_numbers_num) (set! get_prime_numbers_ans (conj get_prime_numbers_ans get_prime_numbers_num))) (set! get_prime_numbers_num (+ get_prime_numbers_num 1)))) (throw (ex-info "return" {:v get_prime_numbers_ans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn prime_factorization [prime_factorization_number]
  (binding [prime_factorization_ans nil prime_factorization_factor nil prime_factorization_quotient nil] (try (do (when (= prime_factorization_number 0) (throw (ex-info "return" {:v [0]}))) (when (= prime_factorization_number 1) (throw (ex-info "return" {:v [1]}))) (set! prime_factorization_ans []) (when (is_prime prime_factorization_number) (do (set! prime_factorization_ans (conj prime_factorization_ans prime_factorization_number)) (throw (ex-info "return" {:v prime_factorization_ans})))) (set! prime_factorization_quotient prime_factorization_number) (set! prime_factorization_factor 2) (while (not= prime_factorization_quotient 1) (if (and (is_prime prime_factorization_factor) (= (mod prime_factorization_quotient prime_factorization_factor) 0)) (do (set! prime_factorization_ans (conj prime_factorization_ans prime_factorization_factor)) (set! prime_factorization_quotient (quot prime_factorization_quotient prime_factorization_factor))) (set! prime_factorization_factor (+ prime_factorization_factor 1)))) (throw (ex-info "return" {:v prime_factorization_ans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn greatest_prime_factor [greatest_prime_factor_number]
  (binding [greatest_prime_factor_factors nil greatest_prime_factor_i nil greatest_prime_factor_m nil] (try (do (set! greatest_prime_factor_factors (prime_factorization greatest_prime_factor_number)) (set! greatest_prime_factor_m (nth greatest_prime_factor_factors 0)) (set! greatest_prime_factor_i 1) (while (< greatest_prime_factor_i (count greatest_prime_factor_factors)) (do (when (> (nth greatest_prime_factor_factors greatest_prime_factor_i) greatest_prime_factor_m) (set! greatest_prime_factor_m (nth greatest_prime_factor_factors greatest_prime_factor_i))) (set! greatest_prime_factor_i (+ greatest_prime_factor_i 1)))) (throw (ex-info "return" {:v greatest_prime_factor_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn smallest_prime_factor [smallest_prime_factor_number]
  (binding [smallest_prime_factor_factors nil smallest_prime_factor_i nil smallest_prime_factor_m nil] (try (do (set! smallest_prime_factor_factors (prime_factorization smallest_prime_factor_number)) (set! smallest_prime_factor_m (nth smallest_prime_factor_factors 0)) (set! smallest_prime_factor_i 1) (while (< smallest_prime_factor_i (count smallest_prime_factor_factors)) (do (when (< (nth smallest_prime_factor_factors smallest_prime_factor_i) smallest_prime_factor_m) (set! smallest_prime_factor_m (nth smallest_prime_factor_factors smallest_prime_factor_i))) (set! smallest_prime_factor_i (+ smallest_prime_factor_i 1)))) (throw (ex-info "return" {:v smallest_prime_factor_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn kg_v [kg_v_number1 kg_v_number2]
  (binding [kg_v_g nil] (try (do (when (or (< kg_v_number1 1) (< kg_v_number2 1)) (throw (Exception. "numbers must be positive"))) (set! kg_v_g (gcd_iter kg_v_number1 kg_v_number2)) (throw (ex-info "return" {:v (* (quot kg_v_number1 kg_v_g) kg_v_number2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_even [is_even_number]
  (try (throw (ex-info "return" {:v (= (mod is_even_number 2) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_odd [is_odd_number]
  (try (throw (ex-info "return" {:v (not= (mod is_odd_number 2) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn goldbach [goldbach_number]
  (binding [goldbach_i nil goldbach_j nil goldbach_primes nil] (try (do (when (or (not (is_even goldbach_number)) (<= goldbach_number 2)) (throw (Exception. "number must be even and > 2"))) (set! goldbach_primes (get_prime_numbers goldbach_number)) (set! goldbach_i 0) (while (< goldbach_i (count goldbach_primes)) (do (set! goldbach_j (+ goldbach_i 1)) (while (< goldbach_j (count goldbach_primes)) (do (when (= (+ (nth goldbach_primes goldbach_i) (nth goldbach_primes goldbach_j)) goldbach_number) (throw (ex-info "return" {:v [(nth goldbach_primes goldbach_i) (nth goldbach_primes goldbach_j)]}))) (set! goldbach_j (+ goldbach_j 1)))) (set! goldbach_i (+ goldbach_i 1)))) (throw (ex-info "return" {:v []}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_prime [get_prime_n]
  (binding [get_prime_ans nil get_prime_index nil] (try (do (when (< get_prime_n 0) (throw (Exception. "n must be non-negative"))) (set! get_prime_index 0) (set! get_prime_ans 2) (while (< get_prime_index get_prime_n) (do (set! get_prime_index (+ get_prime_index 1)) (set! get_prime_ans (+ get_prime_ans 1)) (while (not (is_prime get_prime_ans)) (set! get_prime_ans (+ get_prime_ans 1))))) (throw (ex-info "return" {:v get_prime_ans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_primes_between [get_primes_between_p1 get_primes_between_p2]
  (binding [get_primes_between_ans nil get_primes_between_bad1 nil get_primes_between_bad2 nil get_primes_between_num nil] (try (do (set! get_primes_between_bad1 (not (is_prime get_primes_between_p1))) (set! get_primes_between_bad2 (not (is_prime get_primes_between_p2))) (when (or (or get_primes_between_bad1 get_primes_between_bad2) (>= get_primes_between_p1 get_primes_between_p2)) (throw (Exception. "arguments must be prime and p1 < p2"))) (set! get_primes_between_num (+ get_primes_between_p1 1)) (loop [while_flag_1 true] (when (and while_flag_1 (< get_primes_between_num get_primes_between_p2)) (cond (is_prime get_primes_between_num) (recur false) :else (do (set! get_primes_between_num (+ get_primes_between_num 1)) (recur while_flag_1))))) (set! get_primes_between_ans []) (while (< get_primes_between_num get_primes_between_p2) (do (set! get_primes_between_ans (conj get_primes_between_ans get_primes_between_num)) (set! get_primes_between_num (+ get_primes_between_num 1)) (loop [while_flag_2 true] (when (and while_flag_2 (< get_primes_between_num get_primes_between_p2)) (cond (is_prime get_primes_between_num) (recur false) :else (do (set! get_primes_between_num (+ get_primes_between_num 1)) (recur while_flag_2))))))) (throw (ex-info "return" {:v get_primes_between_ans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_divisors [get_divisors_n]
  (binding [get_divisors_ans nil get_divisors_d nil] (try (do (when (< get_divisors_n 1) (throw (Exception. "n must be >= 1"))) (set! get_divisors_ans []) (set! get_divisors_d 1) (while (<= get_divisors_d get_divisors_n) (do (when (= (mod get_divisors_n get_divisors_d) 0) (set! get_divisors_ans (conj get_divisors_ans get_divisors_d))) (set! get_divisors_d (+ get_divisors_d 1)))) (throw (ex-info "return" {:v get_divisors_ans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_perfect_number [is_perfect_number_number]
  (binding [is_perfect_number_divisors nil is_perfect_number_i nil is_perfect_number_sum nil] (try (do (when (<= is_perfect_number_number 1) (throw (Exception. "number must be > 1"))) (set! is_perfect_number_divisors (get_divisors is_perfect_number_number)) (set! is_perfect_number_sum 0) (set! is_perfect_number_i 0) (while (< is_perfect_number_i (- (count is_perfect_number_divisors) 1)) (do (set! is_perfect_number_sum (+ is_perfect_number_sum (nth is_perfect_number_divisors is_perfect_number_i))) (set! is_perfect_number_i (+ is_perfect_number_i 1)))) (throw (ex-info "return" {:v (= is_perfect_number_sum is_perfect_number_number)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn simplify_fraction [simplify_fraction_numerator simplify_fraction_denominator]
  (binding [simplify_fraction_g nil] (try (do (when (= simplify_fraction_denominator 0) (throw (Exception. "denominator cannot be zero"))) (set! simplify_fraction_g (gcd_iter (abs_int simplify_fraction_numerator) (abs_int simplify_fraction_denominator))) (throw (ex-info "return" {:v [(quot simplify_fraction_numerator simplify_fraction_g) (quot simplify_fraction_denominator simplify_fraction_g)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn factorial [factorial_n]
  (binding [factorial_ans nil factorial_i nil] (try (do (when (< factorial_n 0) (throw (Exception. "n must be >= 0"))) (set! factorial_ans 1) (set! factorial_i 1) (while (<= factorial_i factorial_n) (do (set! factorial_ans (* factorial_ans factorial_i)) (set! factorial_i (+ factorial_i 1)))) (throw (ex-info "return" {:v factorial_ans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fib [fib_n]
  (binding [fib_ans nil fib_fib1 nil fib_i nil fib_tmp nil] (try (do (when (< fib_n 0) (throw (Exception. "n must be >= 0"))) (when (<= fib_n 1) (throw (ex-info "return" {:v 1}))) (set! fib_tmp 0) (set! fib_fib1 1) (set! fib_ans 1) (set! fib_i 0) (while (< fib_i (- fib_n 1)) (do (set! fib_tmp fib_ans) (set! fib_ans (+ fib_ans fib_fib1)) (set! fib_fib1 fib_tmp) (set! fib_i (+ fib_i 1)))) (throw (ex-info "return" {:v fib_ans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_prime 97)))
      (println (str (sieve_er 20)))
      (println (str (get_prime_numbers 20)))
      (println (str (prime_factorization 287)))
      (println (str (greatest_prime_factor 287)))
      (println (str (smallest_prime_factor 287)))
      (println (str (kg_v 8 10)))
      (println (str (goldbach 28)))
      (println (str (get_prime 8)))
      (println (str (get_primes_between 3 20)))
      (println (str (get_divisors 28)))
      (println (str (is_perfect_number 28)))
      (println (str (simplify_fraction 10 20)))
      (println (str (factorial 5)))
      (println (str (fib 10)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
