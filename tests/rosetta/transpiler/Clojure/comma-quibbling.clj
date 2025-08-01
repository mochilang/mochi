(ns main (:refer-clojure :exclude [quibble main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare quibble main)

(declare quibble_n quibble_prefix)

(defn quibble [quibble_items]
  (try (do (def quibble_n (count quibble_items)) (if (= quibble_n 0) (throw (ex-info "return" {:v "{}"})) (if (= quibble_n 1) (throw (ex-info "return" {:v (str (str "{" (nth quibble_items 0)) "}")})) (if (= quibble_n 2) (throw (ex-info "return" {:v (str (str (str (str "{" (nth quibble_items 0)) " and ") (nth quibble_items 1)) "}")})) (do (def quibble_prefix "") (loop [i_seq (- quibble_n 1)] (when (seq i_seq) (let [i (first i_seq)] (cond (= i (- quibble_n 1)) (recur nil) :else (do (when (> i 0) (def quibble_prefix (str quibble_prefix ", "))) (def quibble_prefix (str quibble_prefix (nth quibble_items i))) (recur (rest i_seq))))))) (throw (ex-info "return" {:v (str (str (str (str "{" quibble_prefix) " and ") (nth quibble_items (- quibble_n 1))) "}")}))))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (println (quibble [])) (println (quibble ["ABC"])) (println (quibble ["ABC" "DEF"])) (println (quibble ["ABC" "DEF" "G" "H"]))))

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
