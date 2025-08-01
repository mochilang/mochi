(ns main (:refer-clojure :exclude [sortFloat median fivenum]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sortFloat median fivenum)

(declare arr i j med n q1 q3 s t x1 x2 x3)

(defn sortFloat [xs]
  (try (do (def arr xs) (def n (count arr)) (def i 0) (while (< i n) (do (def j 0) (while (< j (- n 1)) (do (when (> (nth arr j) (nth arr (+' j 1))) (do (def t (nth arr j)) (def arr (assoc arr j (nth arr (+' j 1)))) (def arr (assoc arr (+' j 1) t)))) (def j (+' j 1)))) (def i (+' i 1)))) (throw (ex-info "return" {:v arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn median [s]
  (try (do (def n (count s)) (if (= (mod n 2) 1) (nth s (/ n 2)) (/ (+' (nth s (- (/ n 2) 1)) (nth s (/ n 2))) 2.0))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fivenum [xs]
  (try (do (def s (sortFloat xs)) (def n (count s)) (def q1 (nth s (/ (- n 1) 4))) (def med (median s)) (def q3 (nth s (/ (* 3 (- n 1)) 4))) (throw (ex-info "return" {:v [(nth s 0) q1 med q3 (nth s (- n 1))]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def x1 [36.0 40.0 7.0 39.0 41.0 15.0])

(def x2 [15.0 6.0 42.0 41.0 7.0 36.0 49.0 40.0 39.0 47.0 43.0])

(def x3 [0.14082834 0.0974879 1.73131507 0.87636009 (- 1.95059594) 0.73438555 (- 0.03035726) 1.4667597 (- 0.74621349) (- 0.72588772) 0.6390516 0.61501527 (- 0.9898378) (- 1.00447874) (- 0.62759469) 0.66206163 1.04312009 (- 0.10305385) 0.75775634 0.32566578])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (fivenum x1)))
      (println (str (fivenum x2)))
      (println (str (fivenum x3)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
