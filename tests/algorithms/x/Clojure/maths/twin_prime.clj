(ns main (:refer-clojure :exclude [is_prime twin_prime test_twin_prime main]))

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

(declare is_prime twin_prime test_twin_prime main)

(def ^:dynamic is_prime_i nil)

(defn is_prime [is_prime_n]
  (binding [is_prime_i nil] (try (do (when (< is_prime_n 2) (throw (ex-info "return" {:v false}))) (when (= (mod is_prime_n 2) 0) (throw (ex-info "return" {:v (= is_prime_n 2)}))) (set! is_prime_i 3) (while (<= (* is_prime_i is_prime_i) is_prime_n) (do (when (= (mod is_prime_n is_prime_i) 0) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 2)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn twin_prime [twin_prime_number]
  (try (if (and (is_prime twin_prime_number) (is_prime (+ twin_prime_number 2))) (+ twin_prime_number 2) (- 1)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_twin_prime []
  (do (when (not= (twin_prime 3) 5) (throw (Exception. "twin_prime(3) failed"))) (when (not= (twin_prime 4) (- 1)) (throw (Exception. "twin_prime(4) failed"))) (when (not= (twin_prime 5) 7) (throw (Exception. "twin_prime(5) failed"))) (when (not= (twin_prime 17) 19) (throw (Exception. "twin_prime(17) failed"))) (when (not= (twin_prime 0) (- 1)) (throw (Exception. "twin_prime(0) failed")))))

(defn main []
  (do (test_twin_prime) (println (twin_prime 3))))

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
