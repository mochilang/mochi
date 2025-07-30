(ns main (:refer-clojure :exclude [bellTriangle main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare bellTriangle main)

(defn bellTriangle [n]
  (try (do (def tri []) (def i 0) (while (< i n) (do (def row []) (def j 0) (while (< j i) (do (def row (conj row (bigint 0))) (def j (+' j 1)))) (def tri (conj tri row)) (def i (+' i 1)))) (def tri (assoc-in tri [1 0] 1)) (def i 2) (while (< i n) (do (def tri (assoc-in tri [i 0] (nth (nth tri (- i 1)) (- i 2)))) (def j 1) (while (< j i) (do (def tri (assoc-in tri [i j] (+' (nth (nth tri i) (- j 1)) (nth (nth tri (- i 1)) (- j 1))))) (def j (+' j 1)))) (def i (+' i 1)))) (throw (ex-info "return" {:v tri}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def bt (bellTriangle 51)) (println "First fifteen and fiftieth Bell numbers:") (doseq [i (range 1 16)] (println (str (str (str "" (padStart (str i) 2 " ")) ": ") (str (nth (nth bt i) 0))))) (println (str "50: " (str (nth (nth bt 50) 0)))) (println "") (println "The first ten rows of Bell's triangle:") (doseq [i (range 1 11)] (println (nth bt i)))))

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
