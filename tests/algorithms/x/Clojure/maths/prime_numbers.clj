(ns main (:refer-clojure :exclude [slow_primes primes fast_primes]))

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

(declare slow_primes primes fast_primes)

(def ^:dynamic fast_primes_i nil)

(def ^:dynamic fast_primes_is_prime nil)

(def ^:dynamic fast_primes_j nil)

(def ^:dynamic fast_primes_result nil)

(def ^:dynamic primes_i nil)

(def ^:dynamic primes_is_prime nil)

(def ^:dynamic primes_j nil)

(def ^:dynamic primes_result nil)

(def ^:dynamic slow_primes_i nil)

(def ^:dynamic slow_primes_is_prime nil)

(def ^:dynamic slow_primes_j nil)

(def ^:dynamic slow_primes_result nil)

(defn slow_primes [slow_primes_max_n]
  (binding [slow_primes_i nil slow_primes_is_prime nil slow_primes_j nil slow_primes_result nil] (try (do (set! slow_primes_result []) (set! slow_primes_i 2) (while (<= slow_primes_i slow_primes_max_n) (do (set! slow_primes_j 2) (set! slow_primes_is_prime true) (loop [while_flag_1 true] (when (and while_flag_1 (< slow_primes_j slow_primes_i)) (cond (= (mod slow_primes_i slow_primes_j) 0) (do (set! slow_primes_is_prime false) (recur false)) :else (do (set! slow_primes_j (+ slow_primes_j 1)) (recur while_flag_1))))) (when slow_primes_is_prime (set! slow_primes_result (conj slow_primes_result slow_primes_i))) (set! slow_primes_i (+ slow_primes_i 1)))) (throw (ex-info "return" {:v slow_primes_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn primes [primes_max_n]
  (binding [primes_i nil primes_is_prime nil primes_j nil primes_result nil] (try (do (set! primes_result []) (set! primes_i 2) (while (<= primes_i primes_max_n) (do (set! primes_j 2) (set! primes_is_prime true) (loop [while_flag_2 true] (when (and while_flag_2 (<= (* primes_j primes_j) primes_i)) (cond (= (mod primes_i primes_j) 0) (do (set! primes_is_prime false) (recur false)) :else (do (set! primes_j (+ primes_j 1)) (recur while_flag_2))))) (when primes_is_prime (set! primes_result (conj primes_result primes_i))) (set! primes_i (+ primes_i 1)))) (throw (ex-info "return" {:v primes_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fast_primes [fast_primes_max_n]
  (binding [fast_primes_i nil fast_primes_is_prime nil fast_primes_j nil fast_primes_result nil] (try (do (set! fast_primes_result []) (when (>= fast_primes_max_n 2) (set! fast_primes_result (conj fast_primes_result 2))) (set! fast_primes_i 3) (while (<= fast_primes_i fast_primes_max_n) (do (set! fast_primes_j 3) (set! fast_primes_is_prime true) (loop [while_flag_3 true] (when (and while_flag_3 (<= (* fast_primes_j fast_primes_j) fast_primes_i)) (cond (= (mod fast_primes_i fast_primes_j) 0) (do (set! fast_primes_is_prime false) (recur false)) :else (do (set! fast_primes_j (+ fast_primes_j 2)) (recur while_flag_3))))) (when fast_primes_is_prime (set! fast_primes_result (conj fast_primes_result fast_primes_i))) (set! fast_primes_i (+ fast_primes_i 2)))) (throw (ex-info "return" {:v fast_primes_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (slow_primes 25)))
      (println (str (primes 25)))
      (println (str (fast_primes 25)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
