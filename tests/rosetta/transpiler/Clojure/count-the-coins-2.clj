(ns main (:refer-clojure :exclude [countChange]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare countChange)

(defn countChange [amount]
  (try (do (def ways []) (def i 0) (while (<= i amount) (do (def ways (conj ways 0)) (def i (+ i 1)))) (def ways (assoc ways 0 1)) (doseq [coin [100 50 25 10 5 1]] (do (def j coin) (while (<= j amount) (do (def ways (assoc ways j (+ (nth ways j) (nth ways (- j coin))))) (def j (+ j 1)))))) (throw (ex-info "return" {:v (nth ways amount)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def amount 1000)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (str "amount, ways to make change: " (str amount)) " ") (str (countChange amount))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
