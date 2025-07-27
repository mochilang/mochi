(ns main (:refer-clojure :exclude [angleDiff]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare angleDiff)

(defn angleDiff [b1 b2]
  (try (do (def diff (- b2 b1)) (throw (ex-info "return" {:v (- (mod (+ (+ (mod diff 360) 360) 180) 360) 180)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def testCases [[20 45] [(- 0 45) 45] [(- 0 85) 90] [(- 0 95) 90] [(- 0 45) 125] [(- 0 45) 145] [29.4803 (- 0 88.6381)] [(- 0 78.3251) (- 0 159.036)] [(- 0 70099.74233810938) 29840.67437876723] [(- 0 165313.6666297357) 33693.9894517456] [1174.8380510598456 (- 0 154146.66490124757)] [60175.77306795546 42213.07192354373]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [tc testCases] (println (angleDiff (nth tc 0) (nth tc 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
