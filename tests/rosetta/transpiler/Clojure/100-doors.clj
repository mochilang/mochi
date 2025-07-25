(ns main)

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def doors [])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (dotimes [i 100] (def doors (conj doors false)))
      (doseq [pass (range 1 101)] (do (def idx (- pass 1)) (while (< idx 100) (do (def doors (assoc doors idx (not (nth doors idx)))) (def idx (+ idx pass))))))
      (dotimes [row 10] (do (def line "") (dotimes [col 10] (do (def idx (+ (* row 10) col)) (if (nth doors idx) (def line (str line "1")) (def line (str line "0"))) (when (< col 9) (def line (str line " "))))) (println line)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
