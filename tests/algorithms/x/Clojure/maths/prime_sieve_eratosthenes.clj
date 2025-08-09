(ns main (:refer-clojure :exclude [prime_sieve_eratosthenes list_eq test_prime_sieve_eratosthenes main]))

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

(declare prime_sieve_eratosthenes list_eq test_prime_sieve_eratosthenes main)

(def ^:dynamic list_eq_i nil)

(def ^:dynamic prime_sieve_eratosthenes_i nil)

(def ^:dynamic prime_sieve_eratosthenes_j nil)

(def ^:dynamic prime_sieve_eratosthenes_k nil)

(def ^:dynamic prime_sieve_eratosthenes_p nil)

(def ^:dynamic prime_sieve_eratosthenes_primes nil)

(def ^:dynamic prime_sieve_eratosthenes_result nil)

(defn prime_sieve_eratosthenes [prime_sieve_eratosthenes_num]
  (binding [prime_sieve_eratosthenes_i nil prime_sieve_eratosthenes_j nil prime_sieve_eratosthenes_k nil prime_sieve_eratosthenes_p nil prime_sieve_eratosthenes_primes nil prime_sieve_eratosthenes_result nil] (try (do (when (<= prime_sieve_eratosthenes_num 0) (throw (Exception. "Input must be a positive integer"))) (set! prime_sieve_eratosthenes_primes []) (set! prime_sieve_eratosthenes_i 0) (while (<= prime_sieve_eratosthenes_i prime_sieve_eratosthenes_num) (do (set! prime_sieve_eratosthenes_primes (conj prime_sieve_eratosthenes_primes true)) (set! prime_sieve_eratosthenes_i (+ prime_sieve_eratosthenes_i 1)))) (set! prime_sieve_eratosthenes_p 2) (while (<= (* prime_sieve_eratosthenes_p prime_sieve_eratosthenes_p) prime_sieve_eratosthenes_num) (do (when (nth prime_sieve_eratosthenes_primes prime_sieve_eratosthenes_p) (do (set! prime_sieve_eratosthenes_j (* prime_sieve_eratosthenes_p prime_sieve_eratosthenes_p)) (while (<= prime_sieve_eratosthenes_j prime_sieve_eratosthenes_num) (do (set! prime_sieve_eratosthenes_primes (assoc prime_sieve_eratosthenes_primes prime_sieve_eratosthenes_j false)) (set! prime_sieve_eratosthenes_j (+ prime_sieve_eratosthenes_j prime_sieve_eratosthenes_p)))))) (set! prime_sieve_eratosthenes_p (+ prime_sieve_eratosthenes_p 1)))) (set! prime_sieve_eratosthenes_result []) (set! prime_sieve_eratosthenes_k 2) (while (<= prime_sieve_eratosthenes_k prime_sieve_eratosthenes_num) (do (when (nth prime_sieve_eratosthenes_primes prime_sieve_eratosthenes_k) (set! prime_sieve_eratosthenes_result (conj prime_sieve_eratosthenes_result prime_sieve_eratosthenes_k))) (set! prime_sieve_eratosthenes_k (+ prime_sieve_eratosthenes_k 1)))) (throw (ex-info "return" {:v prime_sieve_eratosthenes_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_eq [list_eq_a list_eq_b]
  (binding [list_eq_i nil] (try (do (when (not= (count list_eq_a) (count list_eq_b)) (throw (ex-info "return" {:v false}))) (set! list_eq_i 0) (while (< list_eq_i (count list_eq_a)) (do (when (not= (nth list_eq_a list_eq_i) (nth list_eq_b list_eq_i)) (throw (ex-info "return" {:v false}))) (set! list_eq_i (+ list_eq_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_prime_sieve_eratosthenes []
  (do (when (not (list_eq (prime_sieve_eratosthenes 10) [2 3 5 7])) (throw (Exception. "test 10 failed"))) (when (not (list_eq (prime_sieve_eratosthenes 20) [2 3 5 7 11 13 17 19])) (throw (Exception. "test 20 failed"))) (when (not (list_eq (prime_sieve_eratosthenes 2) [2])) (throw (Exception. "test 2 failed"))) (when (not= (count (prime_sieve_eratosthenes 1)) 0) (throw (Exception. "test 1 failed")))))

(defn main []
  (do (test_prime_sieve_eratosthenes) (println (str (prime_sieve_eratosthenes 20)))))

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
