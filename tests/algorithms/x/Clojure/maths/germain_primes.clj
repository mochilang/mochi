(ns main (:refer-clojure :exclude [is_prime is_germain_prime is_safe_prime test_is_germain_prime test_is_safe_prime main]))

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

(declare is_prime is_germain_prime is_safe_prime test_is_germain_prime test_is_safe_prime main)

(def ^:dynamic is_prime_i nil)

(defn is_prime [is_prime_n]
  (binding [is_prime_i nil] (try (do (when (<= is_prime_n 1) (throw (ex-info "return" {:v false}))) (when (<= is_prime_n 3) (throw (ex-info "return" {:v true}))) (when (= (mod is_prime_n 2) 0) (throw (ex-info "return" {:v false}))) (set! is_prime_i 3) (while (<= (* is_prime_i is_prime_i) is_prime_n) (do (when (= (mod is_prime_n is_prime_i) 0) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 2)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_germain_prime [is_germain_prime_number]
  (try (do (when (< is_germain_prime_number 1) (throw (Exception. "Input value must be a positive integer"))) (throw (ex-info "return" {:v (and (is_prime is_germain_prime_number) (is_prime (+ (* 2 is_germain_prime_number) 1)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_safe_prime [is_safe_prime_number]
  (try (do (when (< is_safe_prime_number 1) (throw (Exception. "Input value must be a positive integer"))) (if (not= (mod (- is_safe_prime_number 1) 2) 0) false (and (is_prime is_safe_prime_number) (is_prime (quot (- is_safe_prime_number 1) 2))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_is_germain_prime []
  (do (when (not (is_germain_prime 3)) (throw (Exception. "is_germain_prime(3) failed"))) (when (not (is_germain_prime 11)) (throw (Exception. "is_germain_prime(11) failed"))) (when (is_germain_prime 4) (throw (Exception. "is_germain_prime(4) failed"))) (when (not (is_germain_prime 23)) (throw (Exception. "is_germain_prime(23) failed"))) (when (is_germain_prime 13) (throw (Exception. "is_germain_prime(13) failed"))) (when (is_germain_prime 20) (throw (Exception. "is_germain_prime(20) failed")))))

(defn test_is_safe_prime []
  (do (when (not (is_safe_prime 5)) (throw (Exception. "is_safe_prime(5) failed"))) (when (not (is_safe_prime 11)) (throw (Exception. "is_safe_prime(11) failed"))) (when (is_safe_prime 1) (throw (Exception. "is_safe_prime(1) failed"))) (when (is_safe_prime 2) (throw (Exception. "is_safe_prime(2) failed"))) (when (is_safe_prime 3) (throw (Exception. "is_safe_prime(3) failed"))) (when (not (is_safe_prime 47)) (throw (Exception. "is_safe_prime(47) failed")))))

(defn main []
  (do (test_is_germain_prime) (test_is_safe_prime) (println (is_germain_prime 23)) (println (is_safe_prime 47))))

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
