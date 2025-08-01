(ns main (:refer-clojure :exclude [nextRand randBit]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare nextRand randBit)

(declare height line r seed val width x y)

(def width 320)

(def height 240)

(def seed (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) 2147483647))

(defn nextRand []
  (try (do (def seed (mod (+' (* seed 1664525) 1013904223) 2147483647)) (throw (ex-info "return" {:v seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn randBit []
  (try (do (def r (nextRand)) (if (= (mod r 2) 0) 0 255)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def y 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "P2")
      (println (str (str (str width) " ") (str height)))
      (println "255")
      (while (< y height) (do (def line "") (def x 0) (while (< x width) (do (def val (randBit)) (def line (str line (str val))) (when (< x (- width 1)) (def line (str line " "))) (def x (+' x 1)))) (println line) (def y (+' y 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
