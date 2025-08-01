(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main_d main_detailsPerMech main_id main_nMech)

(def main_nMech 5)

(def main_detailsPerMech 4)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [mech (range 1 (+ main_nMech 1))] (do (def main_id mech) (println (str (str (str (str "worker " (str main_id)) " contracted to assemble ") (str main_detailsPerMech)) " details")) (println (str (str "worker " (str main_id)) " enters shop")) (def main_d 0) (while (< main_d main_detailsPerMech) (do (println (str (str "worker " (str main_id)) " assembling")) (println (str (str "worker " (str main_id)) " completed detail")) (def main_d (+ main_d 1)))) (println (str (str "worker " (str main_id)) " leaves shop")) (println (str (str "mechanism " (str mech)) " completed"))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
