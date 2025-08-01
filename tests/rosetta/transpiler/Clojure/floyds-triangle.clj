(ns main (:refer-clojure :exclude [floyd pad]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare floyd pad)

(declare i lastInColumn lastInRow line lowerLeftCorner row t w)

(defn floyd [n]
  (do (println (str (str "Floyd " (str n)) ":")) (def lowerLeftCorner (+' (/ (* n (- n 1)) 2) 1)) (def lastInColumn lowerLeftCorner) (def lastInRow 1) (def i 1) (def row 1) (def line "") (while (<= row n) (do (def w (count (str lastInColumn))) (if (< i lastInRow) (do (def line (str (str line (pad (str i) w)) " ")) (def lastInColumn (+' lastInColumn 1))) (do (def line (str line (pad (str i) w))) (println line) (def line "") (def row (+' row 1)) (def lastInRow (+' lastInRow row)) (def lastInColumn lowerLeftCorner))) (def i (+' i 1))))))

(defn pad [s w]
  (try (do (def t s) (while (< (count t) w) (def t (str " " t))) (throw (ex-info "return" {:v t}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (floyd 5)
      (floyd 14)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
