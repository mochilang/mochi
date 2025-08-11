(ns main (:refer-clojure :exclude [hamming_distance]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare hamming_distance)

(def ^:dynamic count_v nil)

(def ^:dynamic hamming_distance_i nil)

(defn hamming_distance [hamming_distance_a hamming_distance_b]
  (binding [count_v nil hamming_distance_i nil] (try (do (when (not= (count hamming_distance_a) (count hamming_distance_b)) (throw (Exception. "String lengths must match!"))) (set! hamming_distance_i 0) (set! count_v 0) (while (< hamming_distance_i (count hamming_distance_a)) (do (when (not= (subs hamming_distance_a hamming_distance_i (+ hamming_distance_i 1)) (subs hamming_distance_b hamming_distance_i (+ hamming_distance_i 1))) (set! count_v (+ count_v 1))) (set! hamming_distance_i (+ hamming_distance_i 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (hamming_distance "python" "python")))
      (println (str (hamming_distance "karolin" "kathrin")))
      (println (str (hamming_distance "00000" "11111")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
