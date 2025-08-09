(ns main (:refer-clojure :exclude [prime_factors liouville_lambda]))

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

(declare prime_factors liouville_lambda)

(def ^:dynamic liouville_lambda_cnt nil)

(def ^:dynamic prime_factors_factors nil)

(def ^:dynamic prime_factors_i nil)

(def ^:dynamic prime_factors_x nil)

(defn prime_factors [prime_factors_n]
  (binding [prime_factors_factors nil prime_factors_i nil prime_factors_x nil] (try (do (set! prime_factors_i 2) (set! prime_factors_x prime_factors_n) (set! prime_factors_factors []) (while (<= (* prime_factors_i prime_factors_i) prime_factors_x) (if (= (mod prime_factors_x prime_factors_i) 0) (do (set! prime_factors_factors (conj prime_factors_factors prime_factors_i)) (set! prime_factors_x (long (quot prime_factors_x prime_factors_i)))) (set! prime_factors_i (+ prime_factors_i 1)))) (when (> prime_factors_x 1) (set! prime_factors_factors (conj prime_factors_factors prime_factors_x))) (throw (ex-info "return" {:v prime_factors_factors}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn liouville_lambda [liouville_lambda_n]
  (binding [liouville_lambda_cnt nil] (try (do (when (< liouville_lambda_n 1) (throw (Exception. "Input must be a positive integer"))) (set! liouville_lambda_cnt (count (prime_factors liouville_lambda_n))) (if (= (mod liouville_lambda_cnt 2) 0) 1 (- 0 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (liouville_lambda 10))
      (println (liouville_lambda 11))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
