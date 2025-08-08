(ns main (:refer-clojure :exclude [isPrime nextPrime]))

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

(declare isPrime nextPrime)

(def ^:dynamic isPrime_i nil)

(def ^:dynamic nextPrime_firstValue nil)

(def ^:dynamic nextPrime_v nil)

(defn isPrime [isPrime_number]
  (binding [isPrime_i nil] (try (do (when (< isPrime_number 2) (throw (ex-info "return" {:v false}))) (when (< isPrime_number 4) (throw (ex-info "return" {:v true}))) (when (= (mod isPrime_number 2) 0) (throw (ex-info "return" {:v false}))) (set! isPrime_i 3) (while (<= (* isPrime_i isPrime_i) isPrime_number) (do (when (= (mod isPrime_number isPrime_i) 0) (throw (ex-info "return" {:v false}))) (set! isPrime_i (+ isPrime_i 2)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn nextPrime [nextPrime_value nextPrime_factor nextPrime_desc]
  (binding [nextPrime_firstValue nil nextPrime_v nil] (try (do (set! nextPrime_v (* nextPrime_value nextPrime_factor)) (set! nextPrime_firstValue nextPrime_v) (while (not (isPrime nextPrime_v)) (if nextPrime_desc (set! nextPrime_v (- nextPrime_v 1)) (set! nextPrime_v (+ nextPrime_v 1)))) (when (= nextPrime_v nextPrime_firstValue) (if nextPrime_desc (throw (ex-info "return" {:v (nextPrime (- nextPrime_v 1) 1 nextPrime_desc)})) (throw (ex-info "return" {:v (nextPrime (+ nextPrime_v 1) 1 nextPrime_desc)})))) (throw (ex-info "return" {:v nextPrime_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (isPrime 0))
      (println (isPrime 1))
      (println (isPrime 2))
      (println (isPrime 3))
      (println (isPrime 27))
      (println (isPrime 87))
      (println (isPrime 563))
      (println (isPrime 2999))
      (println (isPrime 67483))
      (println (nextPrime 14 1 false))
      (println (nextPrime 14 1 true))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
