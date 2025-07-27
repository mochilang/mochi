(ns main (:refer-clojure :exclude [fib main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare fib main)

(defn fib [n]
  (try (do (when (< n 2) (throw (ex-info "return" {:v n}))) (def a 0) (def b 1) (def i 1) (while (< i n) (do (def t (+ a b)) (def a b) (def b t) (def i (+ i 1)))) (throw (ex-info "return" {:v b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (doseq [n [0 1 2 3 4 5 10 40 (- 1)]] (if (< n 0) (println "fib undefined for negative numbers") (println (str (str (str "fib " (str n)) " = ") (str (fib n)))))))

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
