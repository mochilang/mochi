(ns main (:refer-clojure :exclude [main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main)

(defn main []
  (try (do (def ss {:runtimeFields {}}) (println "Create two fields at runtime: \n") (def i 1) (while (<= i 2) (do (println (str (str "  Field #" (str i)) ":\n")) (println "       Enter name  : ") (def name (read-line)) (println "       Enter value : ") (def value (read-line)) (def fields (:runtimeFields ss)) (def fields (assoc fields name value)) (def ss (assoc ss :runtimeFields fields)) (println "\n") (def i (+ i 1)))) (while true (do (println "Which field do you want to inspect ? ") (def name (read-line)) (if (in name (:runtimeFields ss)) (do (def value (get (:runtimeFields ss) name)) (println (str (str "Its value is '" value) "'")) (throw (ex-info "return" {:v nil}))) (println "There is no field of that name, try again\n"))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

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
