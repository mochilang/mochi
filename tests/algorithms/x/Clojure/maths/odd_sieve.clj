(ns main (:refer-clojure :exclude [odd_sieve]))

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

(declare odd_sieve)

(def ^:dynamic odd_sieve_i nil)

(def ^:dynamic odd_sieve_idx nil)

(def ^:dynamic odd_sieve_j nil)

(def ^:dynamic odd_sieve_j_idx nil)

(def ^:dynamic odd_sieve_k nil)

(def ^:dynamic odd_sieve_n nil)

(def ^:dynamic odd_sieve_primes nil)

(def ^:dynamic odd_sieve_s_idx nil)

(def ^:dynamic odd_sieve_sieve nil)

(def ^:dynamic odd_sieve_size nil)

(defn odd_sieve [odd_sieve_num]
  (binding [odd_sieve_i nil odd_sieve_idx nil odd_sieve_j nil odd_sieve_j_idx nil odd_sieve_k nil odd_sieve_n nil odd_sieve_primes nil odd_sieve_s_idx nil odd_sieve_sieve nil odd_sieve_size nil] (try (do (when (<= odd_sieve_num 2) (throw (ex-info "return" {:v []}))) (when (= odd_sieve_num 3) (throw (ex-info "return" {:v [2]}))) (set! odd_sieve_size (- (quot odd_sieve_num 2) 1)) (set! odd_sieve_sieve []) (set! odd_sieve_idx 0) (while (< odd_sieve_idx odd_sieve_size) (do (set! odd_sieve_sieve (conj odd_sieve_sieve true)) (set! odd_sieve_idx (+ odd_sieve_idx 1)))) (set! odd_sieve_i 3) (while (<= (* odd_sieve_i odd_sieve_i) odd_sieve_num) (do (set! odd_sieve_s_idx (- (quot odd_sieve_i 2) 1)) (when (nth odd_sieve_sieve odd_sieve_s_idx) (do (set! odd_sieve_j (* odd_sieve_i odd_sieve_i)) (while (< odd_sieve_j odd_sieve_num) (do (set! odd_sieve_j_idx (- (quot odd_sieve_j 2) 1)) (set! odd_sieve_sieve (assoc odd_sieve_sieve odd_sieve_j_idx false)) (set! odd_sieve_j (+ odd_sieve_j (* 2 odd_sieve_i))))))) (set! odd_sieve_i (+ odd_sieve_i 2)))) (set! odd_sieve_primes [2]) (set! odd_sieve_n 3) (set! odd_sieve_k 0) (while (< odd_sieve_n odd_sieve_num) (do (when (nth odd_sieve_sieve odd_sieve_k) (set! odd_sieve_primes (conj odd_sieve_primes odd_sieve_n))) (set! odd_sieve_n (+ odd_sieve_n 2)) (set! odd_sieve_k (+ odd_sieve_k 1)))) (throw (ex-info "return" {:v odd_sieve_primes}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (odd_sieve 2))
      (println (odd_sieve 3))
      (println (odd_sieve 10))
      (println (odd_sieve 20))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
