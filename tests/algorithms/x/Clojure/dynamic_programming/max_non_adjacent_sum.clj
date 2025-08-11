(ns main (:refer-clojure :exclude [maximum_non_adjacent_sum]))

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

(declare maximum_non_adjacent_sum)

(def ^:dynamic maximum_non_adjacent_sum_i nil)

(def ^:dynamic maximum_non_adjacent_sum_max_excluding nil)

(def ^:dynamic maximum_non_adjacent_sum_max_including nil)

(def ^:dynamic maximum_non_adjacent_sum_new_excluding nil)

(def ^:dynamic maximum_non_adjacent_sum_new_including nil)

(def ^:dynamic maximum_non_adjacent_sum_num nil)

(defn maximum_non_adjacent_sum [maximum_non_adjacent_sum_nums]
  (binding [maximum_non_adjacent_sum_i nil maximum_non_adjacent_sum_max_excluding nil maximum_non_adjacent_sum_max_including nil maximum_non_adjacent_sum_new_excluding nil maximum_non_adjacent_sum_new_including nil maximum_non_adjacent_sum_num nil] (try (do (when (= (count maximum_non_adjacent_sum_nums) 0) (throw (ex-info "return" {:v 0}))) (set! maximum_non_adjacent_sum_max_including (nth maximum_non_adjacent_sum_nums 0)) (set! maximum_non_adjacent_sum_max_excluding 0) (set! maximum_non_adjacent_sum_i 1) (while (< maximum_non_adjacent_sum_i (count maximum_non_adjacent_sum_nums)) (do (set! maximum_non_adjacent_sum_num (nth maximum_non_adjacent_sum_nums maximum_non_adjacent_sum_i)) (set! maximum_non_adjacent_sum_new_including (+ maximum_non_adjacent_sum_max_excluding maximum_non_adjacent_sum_num)) (set! maximum_non_adjacent_sum_new_excluding (if (> maximum_non_adjacent_sum_max_including maximum_non_adjacent_sum_max_excluding) maximum_non_adjacent_sum_max_including maximum_non_adjacent_sum_max_excluding)) (set! maximum_non_adjacent_sum_max_including maximum_non_adjacent_sum_new_including) (set! maximum_non_adjacent_sum_max_excluding maximum_non_adjacent_sum_new_excluding) (set! maximum_non_adjacent_sum_i (+ maximum_non_adjacent_sum_i 1)))) (if (> maximum_non_adjacent_sum_max_including maximum_non_adjacent_sum_max_excluding) maximum_non_adjacent_sum_max_including maximum_non_adjacent_sum_max_excluding)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (maximum_non_adjacent_sum [1 2 3])))
      (println (str (maximum_non_adjacent_sum [1 5 3 7 2 2 6])))
      (println (str (maximum_non_adjacent_sum [(- 1) (- 5) (- 3) (- 7) (- 2) (- 2) (- 6)])))
      (println (str (maximum_non_adjacent_sum [499 500 (- 3) (- 7) (- 2) (- 2) (- 6)])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
