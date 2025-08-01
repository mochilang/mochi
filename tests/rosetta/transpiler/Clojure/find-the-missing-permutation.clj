(ns main (:refer-clojure :exclude [idx main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare idx main)

(declare ch counts given i j res)

(def given ["ABCD" "CABD" "ACDB" "DACB" "BCDA" "ACBD" "ADCB" "CDAB" "DABC" "BCAD" "CADB" "CDBA" "CBAD" "ABDC" "ADBC" "BDCA" "DCBA" "BACD" "BADC" "BDAC" "CBDA" "DBCA" "DCAB"])

(defn idx [ch]
  (try (do (when (= ch "A") (throw (ex-info "return" {:v 0}))) (when (= ch "B") (throw (ex-info "return" {:v 1}))) (if (= ch "C") 2 3)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def res "") (def i 0) (while (< i (count (nth given 0))) (do (def counts [0 0 0 0]) (doseq [p given] (do (def ch (subs p i (+' i 1))) (def j (idx ch)) (def counts (assoc counts j (+' (nth counts j) 1))))) (def j 0) (while (< j 4) (do (when (= (mod (nth counts j) 2) 1) (if (= j 0) (def res (str res "A")) (if (= j 1) (def res (str res "B")) (if (= j 2) (def res (str res "C")) (def res (str res "D")))))) (def j (+' j 1)))) (def i (+' i 1)))) (println res)))

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
