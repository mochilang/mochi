(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def arr1 [2 7 1 8 2])
      (def counts1 {})
      (def keys1 [])
      (def i 0)
      (while (< i (count arr1)) (do (def v (nth arr1 i)) (if (in v counts1) (def counts1 (assoc counts1 v (+ (get counts1 v) 1))) (do (def counts1 (assoc counts1 v 1)) (def keys1 (conj keys1 v)))) (def i (+ i 1))))
      (def max1 0)
      (def i 0)
      (while (< i (count keys1)) (do (def k (nth keys1 i)) (def c (get counts1 k)) (when (> c max1) (def max1 c)) (def i (+ i 1))))
      (def modes1 [])
      (def i 0)
      (while (< i (count keys1)) (do (def k (nth keys1 i)) (when (= (get counts1 k) max1) (def modes1 (conj modes1 k))) (def i (+ i 1))))
      (println (str modes1))
      (def arr2 [2 7 1 8 2 8])
      (def counts2 {})
      (def keys2 [])
      (def i 0)
      (while (< i (count arr2)) (do (def v (nth arr2 i)) (if (in v counts2) (def counts2 (assoc counts2 v (+ (get counts2 v) 1))) (do (def counts2 (assoc counts2 v 1)) (def keys2 (conj keys2 v)))) (def i (+ i 1))))
      (def max2 0)
      (def i 0)
      (while (< i (count keys2)) (do (def k (nth keys2 i)) (def c (get counts2 k)) (when (> c max2) (def max2 c)) (def i (+ i 1))))
      (def modes2 [])
      (def i 0)
      (while (< i (count keys2)) (do (def k (nth keys2 i)) (when (= (get counts2 k) max2) (def modes2 (conj modes2 k))) (def i (+ i 1))))
      (println (str modes2))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
