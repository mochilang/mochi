(ns main (:refer-clojure :exclude [wheatstone_solver]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare wheatstone_solver)

(defn wheatstone_solver [wheatstone_solver_resistance_1 wheatstone_solver_resistance_2 wheatstone_solver_resistance_3]
  (try (do (when (or (or (<= wheatstone_solver_resistance_1 0.0) (<= wheatstone_solver_resistance_2 0.0)) (<= wheatstone_solver_resistance_3 0.0)) (throw (Exception. "All resistance values must be positive"))) (throw (ex-info "return" {:v (* (quot wheatstone_solver_resistance_2 wheatstone_solver_resistance_1) wheatstone_solver_resistance_3)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (wheatstone_solver 2.0 4.0 5.0))
      (println (wheatstone_solver 356.0 234.0 976.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
