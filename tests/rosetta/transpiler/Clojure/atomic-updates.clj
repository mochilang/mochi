(ns main (:refer-clojure :exclude [randOrder randChaos main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare randOrder randChaos main)

(defn randOrder [seed n]
  (try (do (def next (mod (+ (* seed 1664525) 1013904223) 2147483647)) (throw (ex-info "return" {:v [next (mod next n)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn randChaos [seed n]
  (try (do (def next (mod (+ (* seed 1103515245) 12345) 2147483647)) (throw (ex-info "return" {:v [next (mod next n)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def nBuckets 10) (def initialSum 1000) (def buckets []) (dotimes [i nBuckets] (def buckets (conj buckets 0))) (def i nBuckets) (def dist initialSum) (while (> i 0) (do (def v (/ dist i)) (def i (- i 1)) (def buckets (assoc buckets i v)) (def dist (- dist v)))) (def tc0 0) (def tc1 0) (def total 0) (def nTicks 0) (def seedOrder 1) (def seedChaos 2) (println "sum  ---updates---    mean  buckets") (def t 0) (while (< t 5) (do (def r (randOrder seedOrder nBuckets)) (def seedOrder (nth r 0)) (def b1 (nth r 1)) (def b2 (mod (+ b1 1) nBuckets)) (def v1 (nth buckets b1)) (def v2 (nth buckets b2)) (if (> v1 v2) (do (def a (int (/ (- v1 v2) 2))) (when (> a (nth buckets b1)) (def a (nth buckets b1))) (def buckets (assoc buckets b1 (- (nth buckets b1) a))) (def buckets (assoc buckets b2 (+ (nth buckets b2) a)))) (do (def a (int (/ (- v2 v1) 2))) (when (> a (nth buckets b2)) (def a (nth buckets b2))) (def buckets (assoc buckets b2 (- (nth buckets b2) a))) (def buckets (assoc buckets b1 (+ (nth buckets b1) a))))) (def tc0 (+ tc0 1)) (def r (randChaos seedChaos nBuckets)) (def seedChaos (nth r 0)) (def b1 (nth r 1)) (def b2 (mod (+ b1 1) nBuckets)) (def r (randChaos seedChaos (+ (nth buckets b1) 1))) (def seedChaos (nth r 0)) (def amt (nth r 1)) (when (> amt (nth buckets b1)) (def amt (nth buckets b1))) (def buckets (assoc buckets b1 (- (nth buckets b1) amt))) (def buckets (assoc buckets b2 (+ (nth buckets b2) amt))) (def tc1 (+ tc1 1)) (def sum 0) (def idx 0) (while (< idx nBuckets) (do (def sum (+ sum (nth buckets idx))) (def idx (+ idx 1)))) (def total (+ (+ total tc0) tc1)) (def nTicks (+ nTicks 1)) (println (str (str (str (str (str (str (str (str (str sum) " ") (str tc0)) " ") (str tc1)) " ") (str (/ total nTicks))) "  ") (str buckets))) (def tc0 0) (def tc1 0) (def t (+ t 1))))))

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
