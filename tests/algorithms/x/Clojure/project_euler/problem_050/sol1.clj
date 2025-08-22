(ns main (:refer-clojure :exclude [prime_sieve solution]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare prime_sieve solution)

(declare _read_file)

(def ^:dynamic prime_sieve_i nil)

(def ^:dynamic prime_sieve_index nil)

(def ^:dynamic prime_sieve_is_prime nil)

(def ^:dynamic prime_sieve_n nil)

(def ^:dynamic prime_sieve_p nil)

(def ^:dynamic prime_sieve_primes nil)

(def ^:dynamic solution_L nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_max_len nil)

(def ^:dynamic solution_prefix nil)

(def ^:dynamic solution_prime_map nil)

(def ^:dynamic solution_primes nil)

(def ^:dynamic solution_s nil)

(def ^:dynamic solution_start nil)

(defn prime_sieve [prime_sieve_limit]
  (binding [prime_sieve_i nil prime_sieve_index nil prime_sieve_is_prime nil prime_sieve_n nil prime_sieve_p nil prime_sieve_primes nil] (try (do (when (<= prime_sieve_limit 2) (throw (ex-info "return" {:v []}))) (set! prime_sieve_is_prime []) (set! prime_sieve_i 0) (while (< prime_sieve_i prime_sieve_limit) (do (set! prime_sieve_is_prime (conj prime_sieve_is_prime true)) (set! prime_sieve_i (+ prime_sieve_i 1)))) (set! prime_sieve_is_prime (assoc prime_sieve_is_prime 0 false)) (set! prime_sieve_is_prime (assoc prime_sieve_is_prime 1 false)) (set! prime_sieve_p 3) (while (< (* prime_sieve_p prime_sieve_p) prime_sieve_limit) (do (set! prime_sieve_index (* prime_sieve_p 2)) (while (< prime_sieve_index prime_sieve_limit) (do (set! prime_sieve_is_prime (assoc prime_sieve_is_prime prime_sieve_index false)) (set! prime_sieve_index (+ prime_sieve_index prime_sieve_p)))) (set! prime_sieve_p (+ prime_sieve_p 2)))) (set! prime_sieve_primes [2]) (set! prime_sieve_n 3) (while (< prime_sieve_n prime_sieve_limit) (do (when (nth prime_sieve_is_prime prime_sieve_n) (set! prime_sieve_primes (conj prime_sieve_primes prime_sieve_n))) (set! prime_sieve_n (+ prime_sieve_n 2)))) (throw (ex-info "return" {:v prime_sieve_primes}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_ceiling]
  (binding [solution_L nil solution_i nil solution_max_len nil solution_prefix nil solution_prime_map nil solution_primes nil solution_s nil solution_start nil] (try (do (set! solution_primes (prime_sieve solution_ceiling)) (set! solution_prime_map {}) (set! solution_i 0) (while (< solution_i (count solution_primes)) (do (set! solution_prime_map (assoc solution_prime_map (nth solution_primes solution_i) true)) (set! solution_i (+ solution_i 1)))) (set! solution_prefix [0]) (set! solution_i 0) (while (< solution_i (count solution_primes)) (do (set! solution_prefix (conj solution_prefix (+ (nth solution_prefix solution_i) (nth solution_primes solution_i)))) (set! solution_i (+ solution_i 1)))) (set! solution_max_len 0) (while (and (< solution_max_len (count solution_prefix)) (< (nth solution_prefix solution_max_len) solution_ceiling)) (set! solution_max_len (+ solution_max_len 1))) (set! solution_L solution_max_len) (loop [while_flag_1 true] (when (and while_flag_1 (> solution_L 0)) (do (set! solution_start 0) (loop [while_flag_2 true] (when (and while_flag_2 (<= (+ solution_start solution_L) (count solution_primes))) (do (set! solution_s (- (nth solution_prefix (+ solution_start solution_L)) (nth solution_prefix solution_start))) (cond (>= solution_s solution_ceiling) (recur false) :else (do (when (get solution_prime_map solution_s) (throw (ex-info "return" {:v solution_s}))) (set! solution_start (+ solution_start 1)) (recur while_flag_2)))))) (set! solution_L (- solution_L 1)) (cond :else (do))))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_ans nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_ans) (constantly (solution 1000000)))
      (println (str "solution() = " (mochi_str main_ans)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
