(ns main (:refer-clojure :exclude [applyFilter]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare applyFilter)

(defn applyFilter [input a b]
  (try (do (def out []) (def scale (/ 1 (nth a 0))) (def i 0) (while (< i (count input)) (do (def tmp 0) (def j 0) (while (and (<= j i) (< j (count b))) (do (def tmp (+ tmp (* (nth b j) (nth input (- i j))))) (def j (+ j 1)))) (def j 0) (while (and (< j i) (< (+ j 1) (count a))) (do (def tmp (- tmp (* (nth a (+ j 1)) (nth out (- (- i j) 1))))) (def j (+ j 1)))) (def out (conj out (* tmp scale))) (def i (+ i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def a [1 (- 0.00000000000000027756) 0.33333333 (- 0.0000000000000000185)])

(def b [0.16666667 0.5 0.5 0.16666667])

(def sig [(- 0.917843918645) 0.141984778794 1.20536903482 0.190286794412 (- 0.662370894973) (- 1.00700480494) (- 0.404707073677) 0.800482325044 0.743500089861 1.01090520172 0.741527555207 0.277841675195 0.400833448236 (- 0.2085993586) (- 0.172842103641) (- 0.134316096293) 0.0259303398477 0.490105989562 0.549391221511 0.9047198589])

(def res (applyFilter sig a b))

(def k 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< k (count res)) (do (println (nth res k)) (def k (+ k 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
