(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def msg "Hello World! ")

(def shift 0)

(def inc 1)

(def clicks 0)

(def frames 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< clicks 5) (do (def line "") (def i 0) (while (< i (count msg)) (do (def idx (mod (+ shift i) (count msg))) (def line (str line (subs msg idx (+ idx 1)))) (def i (+ i 1)))) (println line) (def shift (mod (+ shift inc) (count msg))) (def frames (+ frames 1)) (when (= (mod frames (count msg)) 0) (do (def inc (- (count msg) inc)) (def clicks (+ clicks 1))))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
