(ns main (:refer-clojure :exclude [isqrt prime_sieve]))

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

(declare isqrt prime_sieve)

(def ^:dynamic isqrt_r nil)

(def ^:dynamic prime_sieve_end nil)

(def ^:dynamic prime_sieve_i nil)

(def ^:dynamic prime_sieve_j nil)

(def ^:dynamic prime_sieve_k nil)

(def ^:dynamic prime_sieve_prime nil)

(def ^:dynamic prime_sieve_sieve nil)

(def ^:dynamic prime_sieve_start nil)

(defn isqrt [isqrt_n]
  (binding [isqrt_r nil] (try (do (set! isqrt_r 0) (while (<= (* (+ isqrt_r 1) (+ isqrt_r 1)) isqrt_n) (set! isqrt_r (+ isqrt_r 1))) (throw (ex-info "return" {:v isqrt_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn prime_sieve [prime_sieve_num]
  (binding [prime_sieve_end nil prime_sieve_i nil prime_sieve_j nil prime_sieve_k nil prime_sieve_prime nil prime_sieve_sieve nil prime_sieve_start nil] (try (do (when (<= prime_sieve_num 0) (throw (Exception. "Invalid input, please enter a positive integer."))) (set! prime_sieve_sieve []) (set! prime_sieve_i 0) (while (<= prime_sieve_i prime_sieve_num) (do (set! prime_sieve_sieve (conj prime_sieve_sieve true)) (set! prime_sieve_i (+ prime_sieve_i 1)))) (set! prime_sieve_prime []) (set! prime_sieve_start 2) (set! prime_sieve_end (isqrt prime_sieve_num)) (while (<= prime_sieve_start prime_sieve_end) (do (when (nth prime_sieve_sieve prime_sieve_start) (do (set! prime_sieve_prime (conj prime_sieve_prime prime_sieve_start)) (set! prime_sieve_j (* prime_sieve_start prime_sieve_start)) (while (<= prime_sieve_j prime_sieve_num) (do (when (nth prime_sieve_sieve prime_sieve_j) (set! prime_sieve_sieve (assoc prime_sieve_sieve prime_sieve_j false))) (set! prime_sieve_j (+ prime_sieve_j prime_sieve_start)))))) (set! prime_sieve_start (+ prime_sieve_start 1)))) (set! prime_sieve_k (+ prime_sieve_end 1)) (while (<= prime_sieve_k prime_sieve_num) (do (when (nth prime_sieve_sieve prime_sieve_k) (set! prime_sieve_prime (conj prime_sieve_prime prime_sieve_k))) (set! prime_sieve_k (+ prime_sieve_k 1)))) (throw (ex-info "return" {:v prime_sieve_prime}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (prime_sieve 50)))
      (println (str (prime_sieve 25)))
      (println (str (prime_sieve 10)))
      (println (str (prime_sieve 9)))
      (println (str (prime_sieve 2)))
      (println (str (prime_sieve 1)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
