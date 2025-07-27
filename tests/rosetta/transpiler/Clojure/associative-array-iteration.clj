(ns main (:refer-clojure :exclude [main]))

(require 'clojure.set)

(defrecord M [hello world !])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main)

(defn main []
  (do (def m {"hello" 13 "world" 31 "!" 71}) (doseq [k (keys m)] (println (str (str (str "key = " k) ", value = ") (str (nth m k))))) (doseq [k (keys m)] (println (str "key = " k))) (doseq [k (keys m)] (println (str "value = " (str (nth m k)))))))

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
