(ns main (:refer-clojure :exclude [is_prime]))

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

(declare is_prime)

(def ^:dynamic is_prime_i nil)

(defn is_prime [is_prime_number]
  (binding [is_prime_i nil] (try (do (when (< is_prime_number 0) (throw (Exception. "is_prime() only accepts positive integers"))) (when (< is_prime_number 2) (throw (ex-info "return" {:v false}))) (when (< is_prime_number 4) (throw (ex-info "return" {:v true}))) (when (or (= (mod is_prime_number 2) 0) (= (mod is_prime_number 3) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i 5) (while (<= (* is_prime_i is_prime_i) is_prime_number) (do (when (or (= (mod is_prime_number is_prime_i) 0) (= (mod is_prime_number (+ is_prime_i 2)) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 6)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_prime 2)))
      (println (str (is_prime 3)))
      (println (str (is_prime 5)))
      (println (str (is_prime 7)))
      (println (str (is_prime 11)))
      (println (str (is_prime 13)))
      (println (str (is_prime 17)))
      (println (str (is_prime 19)))
      (println (str (is_prime 23)))
      (println (str (is_prime 29)))
      (println (str (is_prime 0)))
      (println (str (is_prime 1)))
      (println (str (is_prime 4)))
      (println (str (is_prime 6)))
      (println (str (is_prime 9)))
      (println (str (is_prime 15)))
      (println (str (is_prime 105)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
