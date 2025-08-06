(ns main (:refer-clojure :exclude [backtrack combination_sum]))

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

(declare backtrack combination_sum)

(def ^:dynamic backtrack_i nil)

(def ^:dynamic backtrack_new_path nil)

(def ^:dynamic backtrack_result nil)

(def ^:dynamic backtrack_value nil)

(def ^:dynamic combination_sum_path nil)

(def ^:dynamic combination_sum_result nil)

(defn backtrack [backtrack_candidates backtrack_start backtrack_target backtrack_path backtrack_result_p]
  (binding [backtrack_i nil backtrack_new_path nil backtrack_result nil backtrack_value nil] (try (do (set! backtrack_result backtrack_result_p) (when (= backtrack_target 0) (throw (ex-info "return" {:v (conj backtrack_result backtrack_path)}))) (set! backtrack_i backtrack_start) (while (< backtrack_i (count backtrack_candidates)) (do (set! backtrack_value (nth backtrack_candidates backtrack_i)) (when (<= backtrack_value backtrack_target) (do (set! backtrack_new_path (conj backtrack_path backtrack_value)) (set! backtrack_result (backtrack backtrack_candidates backtrack_i (- backtrack_target backtrack_value) backtrack_new_path backtrack_result)))) (set! backtrack_i (+ backtrack_i 1)))) (throw (ex-info "return" {:v backtrack_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn combination_sum [combination_sum_candidates combination_sum_target]
  (binding [combination_sum_path nil combination_sum_result nil] (try (do (set! combination_sum_path []) (set! combination_sum_result []) (throw (ex-info "return" {:v (backtrack combination_sum_candidates 0 combination_sum_target combination_sum_path combination_sum_result)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (combination_sum [2 3 5] 8)))
      (println (str (combination_sum [2 3 6 7] 7)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
