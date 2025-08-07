(ns main (:refer-clojure :exclude [int_pow pow_mod rand_range rabin_miller is_prime_low_num generate_large_prime]))

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

(declare int_pow pow_mod rand_range rabin_miller is_prime_low_num generate_large_prime)

(def ^:dynamic generate_large_prime_end nil)

(def ^:dynamic generate_large_prime_num nil)

(def ^:dynamic generate_large_prime_start nil)

(def ^:dynamic int_pow_i nil)

(def ^:dynamic int_pow_result nil)

(def ^:dynamic is_prime_low_num_i nil)

(def ^:dynamic is_prime_low_num_low_primes nil)

(def ^:dynamic is_prime_low_num_p nil)

(def ^:dynamic pow_mod_b nil)

(def ^:dynamic pow_mod_e nil)

(def ^:dynamic pow_mod_result nil)

(def ^:dynamic rabin_miller_a nil)

(def ^:dynamic rabin_miller_i nil)

(def ^:dynamic rabin_miller_k nil)

(def ^:dynamic rabin_miller_s nil)

(def ^:dynamic rabin_miller_t nil)

(def ^:dynamic rabin_miller_v nil)

(defn int_pow [int_pow_base int_pow_exp]
  (binding [int_pow_i nil int_pow_result nil] (try (do (set! int_pow_result 1) (set! int_pow_i 0) (while (< int_pow_i int_pow_exp) (do (set! int_pow_result (* int_pow_result int_pow_base)) (set! int_pow_i (+ int_pow_i 1)))) (throw (ex-info "return" {:v int_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow_mod [pow_mod_base pow_mod_exp pow_mod_mod]
  (binding [pow_mod_b nil pow_mod_e nil pow_mod_result nil] (try (do (set! pow_mod_result 1) (set! pow_mod_b (mod pow_mod_base pow_mod_mod)) (set! pow_mod_e pow_mod_exp) (while (> pow_mod_e 0) (do (when (= (mod pow_mod_e 2) 1) (set! pow_mod_result (mod (* pow_mod_result pow_mod_b) pow_mod_mod))) (set! pow_mod_e (quot pow_mod_e 2)) (set! pow_mod_b (mod (* pow_mod_b pow_mod_b) pow_mod_mod)))) (throw (ex-info "return" {:v pow_mod_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rand_range [rand_range_low rand_range_high]
  (try (throw (ex-info "return" {:v (+ (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) (- rand_range_high rand_range_low)) rand_range_low)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rabin_miller [rabin_miller_num]
  (binding [rabin_miller_a nil rabin_miller_i nil rabin_miller_k nil rabin_miller_s nil rabin_miller_t nil rabin_miller_v nil] (try (do (set! rabin_miller_s (- rabin_miller_num 1)) (set! rabin_miller_t 0) (while (= (mod rabin_miller_s 2) 0) (do (set! rabin_miller_s (quot rabin_miller_s 2)) (set! rabin_miller_t (+ rabin_miller_t 1)))) (set! rabin_miller_k 0) (while (< rabin_miller_k 5) (do (set! rabin_miller_a (rand_range 2 (- rabin_miller_num 1))) (set! rabin_miller_v (pow_mod rabin_miller_a rabin_miller_s rabin_miller_num)) (when (not= rabin_miller_v 1) (do (set! rabin_miller_i 0) (while (not= rabin_miller_v (- rabin_miller_num 1)) (do (when (= rabin_miller_i (- rabin_miller_t 1)) (throw (ex-info "return" {:v false}))) (set! rabin_miller_i (+ rabin_miller_i 1)) (set! rabin_miller_v (mod (* rabin_miller_v rabin_miller_v) rabin_miller_num)))))) (set! rabin_miller_k (+ rabin_miller_k 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_prime_low_num [is_prime_low_num_num]
  (binding [is_prime_low_num_i nil is_prime_low_num_low_primes nil is_prime_low_num_p nil] (try (do (when (< is_prime_low_num_num 2) (throw (ex-info "return" {:v false}))) (set! is_prime_low_num_low_primes [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997]) (when (in is_prime_low_num_num is_prime_low_num_low_primes) (throw (ex-info "return" {:v true}))) (set! is_prime_low_num_i 0) (while (< is_prime_low_num_i (count is_prime_low_num_low_primes)) (do (set! is_prime_low_num_p (nth is_prime_low_num_low_primes is_prime_low_num_i)) (when (= (mod is_prime_low_num_num is_prime_low_num_p) 0) (throw (ex-info "return" {:v false}))) (set! is_prime_low_num_i (+ is_prime_low_num_i 1)))) (throw (ex-info "return" {:v (rabin_miller is_prime_low_num_num)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_large_prime [generate_large_prime_keysize]
  (binding [generate_large_prime_end nil generate_large_prime_num nil generate_large_prime_start nil] (try (do (set! generate_large_prime_start (int_pow 2 (- generate_large_prime_keysize 1))) (set! generate_large_prime_end (int_pow 2 generate_large_prime_keysize)) (while true (do (set! generate_large_prime_num (rand_range generate_large_prime_start generate_large_prime_end)) (when (is_prime_low_num generate_large_prime_num) (throw (ex-info "return" {:v generate_large_prime_num})))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_p (generate_large_prime 16))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "Prime number: " (str main_p)))
      (println (str "is_prime_low_num: " (str (is_prime_low_num main_p))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
