(ns main (:refer-clojure :exclude [countDivisors main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare countDivisors main)

(defn countDivisors [n]
  (try (do (when (< n 2) (throw (ex-info "return" {:v 1}))) (def count_v 2) (def i 2) (while (<= i (/ n 2)) (do (when (= (mod n i) 0) (def count_v (+ count_v 1))) (def i (+ i 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (println "The first 20 anti-primes are:") (def maxDiv 0) (def count_v 0) (def n 1) (def line "") (while (< count_v 20) (do (def d (countDivisors n)) (when (> d maxDiv) (do (def line (str (str line (str n)) " ")) (def maxDiv d) (def count_v (+ count_v 1)))) (def n (+ n 1)))) (def line (subs line 0 (- (count line) 1))) (println line)))

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
