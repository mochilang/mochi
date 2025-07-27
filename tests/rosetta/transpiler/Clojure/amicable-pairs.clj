(ns main (:refer-clojure :exclude [pfacSum pad main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pfacSum pad main)

(defn pfacSum [i]
  (try (do (def sum 0) (def p 1) (while (<= p (/ i 2)) (do (when (= (mod i p) 0) (def sum (+ sum p))) (def p (+ p 1)))) (throw (ex-info "return" {:v sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [n width]
  (try (do (def s (str n)) (while (< (count s) width) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def sums []) (def i 0) (while (< i 20000) (do (def sums (conj sums 0)) (def i (+ i 1)))) (def i 1) (while (< i 20000) (do (def sums (assoc sums i (pfacSum i))) (def i (+ i 1)))) (println "The amicable pairs below 20,000 are:") (def n 2) (while (< n 19999) (do (def m (nth sums n)) (when (and (and (> m n) (< m 20000)) (= n (nth sums m))) (println (str (str (str "  " (pad n 5)) " and ") (pad m 5)))) (def n (+ n 1))))))

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
