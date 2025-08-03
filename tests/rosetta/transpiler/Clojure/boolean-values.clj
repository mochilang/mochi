(ns main (:refer-clojure :exclude [parseBool main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare parseBool main)

(declare main_bolStr main_n main_str1 main_x main_y parseBool_l)

(defn parseBool [parseBool_s]
  (try (do (def parseBool_l (clojure.string/lower-case parseBool_s)) (if (or (or (or (or (= parseBool_l "1") (= parseBool_l "t")) (= parseBool_l true)) (= parseBool_l "yes")) (= parseBool_l "y")) true false)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_n true) (println main_n) (println "bool") (def main_n (not main_n)) (println main_n) (def main_x 5) (def main_y 8) (println "x == y:" (= main_x main_y)) (println "x < y:" (< main_x main_y)) (println "\nConvert String into Boolean Data type\n") (def main_str1 "japan") (println "Before :" "string") (def main_bolStr (parseBool main_str1)) (println "After :" "bool")))

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
