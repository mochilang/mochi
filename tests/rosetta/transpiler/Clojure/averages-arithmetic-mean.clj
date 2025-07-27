(ns main (:refer-clojure :exclude [mean main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare mean main)

(defn mean [v]
  (try (do (when (= (count v) 0) (throw (ex-info "return" {:v {"ok" false}}))) (def sum 0.0) (def i 0) (while (< i (count v)) (do (def sum (+ sum (nth v i))) (def i (+ i 1)))) (throw (ex-info "return" {:v {"ok" true "mean" (/ sum (double (count v)))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def sets [[] [3.0 1.0 4.0 1.0 5.0 9.0] [100000000000000000000.0 3.0 1.0 4.0 1.0 5.0 9.0 (- 100000000000000000000.0)] [10.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0 0.0 0.0 0.0 0.0 0.11] [10.0 20.0 30.0 40.0 50.0 (- 100.0) 4.7 (- 1100.0)]]) (doseq [v sets] (do (println (str "Vector: " (str v))) (def r (mean v)) (if (get r "ok") (println (str (str (str "Mean of " (str (count v))) " numbers is ") (str (get r "mean")))) (println "Mean undefined")) (println "")))))

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
