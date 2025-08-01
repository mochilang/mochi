(ns main (:refer-clojure :exclude [printStat main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare printStat main)

(declare main_fs)

(defn printStat [printStat_fs printStat_path]
  (if (in printStat_path printStat_fs) (if (get printStat_fs printStat_path) (println (str printStat_path " is a directory")) (println (str printStat_path " is a file"))) (println (str (str "stat " printStat_path) ": no such file or directory"))))

(defn main []
  (do (def main_fs {}) (def main_fs (assoc main_fs "docs" true)) (doseq [p ["input.txt" "/input.txt" "docs" "/docs"]] (printStat main_fs p))))

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
