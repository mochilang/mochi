(ns main (:refer-clojure :exclude [index_of_min remove_at optimal_merge_pattern]))

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

(declare index_of_min remove_at optimal_merge_pattern)

(def ^:dynamic index_of_min_i nil)

(def ^:dynamic index_of_min_min_idx nil)

(def ^:dynamic optimal_merge_pattern_arr nil)

(def ^:dynamic optimal_merge_pattern_k nil)

(def ^:dynamic optimal_merge_pattern_min_idx nil)

(def ^:dynamic optimal_merge_pattern_optimal_merge_cost nil)

(def ^:dynamic optimal_merge_pattern_temp nil)

(def ^:dynamic remove_at_i nil)

(def ^:dynamic remove_at_res nil)

(defn index_of_min [index_of_min_xs]
  (binding [index_of_min_i nil index_of_min_min_idx nil] (try (do (set! index_of_min_min_idx 0) (set! index_of_min_i 1) (while (< index_of_min_i (count index_of_min_xs)) (do (when (< (nth index_of_min_xs index_of_min_i) (nth index_of_min_xs index_of_min_min_idx)) (set! index_of_min_min_idx index_of_min_i)) (set! index_of_min_i (+ index_of_min_i 1)))) (throw (ex-info "return" {:v index_of_min_min_idx}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_at [remove_at_xs remove_at_idx]
  (binding [remove_at_i nil remove_at_res nil] (try (do (set! remove_at_res []) (set! remove_at_i 0) (while (< remove_at_i (count remove_at_xs)) (do (when (not= remove_at_i remove_at_idx) (set! remove_at_res (conj remove_at_res (nth remove_at_xs remove_at_i)))) (set! remove_at_i (+ remove_at_i 1)))) (throw (ex-info "return" {:v remove_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn optimal_merge_pattern [optimal_merge_pattern_files]
  (binding [optimal_merge_pattern_arr nil optimal_merge_pattern_k nil optimal_merge_pattern_min_idx nil optimal_merge_pattern_optimal_merge_cost nil optimal_merge_pattern_temp nil] (try (do (set! optimal_merge_pattern_arr optimal_merge_pattern_files) (set! optimal_merge_pattern_optimal_merge_cost 0) (while (> (count optimal_merge_pattern_arr) 1) (do (set! optimal_merge_pattern_temp 0) (set! optimal_merge_pattern_k 0) (while (< optimal_merge_pattern_k 2) (do (set! optimal_merge_pattern_min_idx (index_of_min optimal_merge_pattern_arr)) (set! optimal_merge_pattern_temp (+ optimal_merge_pattern_temp (nth optimal_merge_pattern_arr optimal_merge_pattern_min_idx))) (set! optimal_merge_pattern_arr (remove_at optimal_merge_pattern_arr optimal_merge_pattern_min_idx)) (set! optimal_merge_pattern_k (+ optimal_merge_pattern_k 1)))) (set! optimal_merge_pattern_arr (conj optimal_merge_pattern_arr optimal_merge_pattern_temp)) (set! optimal_merge_pattern_optimal_merge_cost (+ optimal_merge_pattern_optimal_merge_cost optimal_merge_pattern_temp)))) (throw (ex-info "return" {:v optimal_merge_pattern_optimal_merge_cost}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (optimal_merge_pattern [2 3 4]))
      (println (optimal_merge_pattern [5 10 20 30 30]))
      (println (optimal_merge_pattern [8 8 8 8 8]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
