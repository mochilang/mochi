(ns main (:refer-clojure :exclude [max_subarray_sum]))

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

(declare max_subarray_sum)

(def ^:dynamic max_subarray_sum_curr_sum nil)

(def ^:dynamic max_subarray_sum_i nil)

(def ^:dynamic max_subarray_sum_max_sum nil)

(def ^:dynamic max_subarray_sum_num nil)

(def ^:dynamic max_subarray_sum_temp nil)

(defn max_subarray_sum [max_subarray_sum_nums max_subarray_sum_allow_empty]
  (binding [max_subarray_sum_curr_sum nil max_subarray_sum_i nil max_subarray_sum_max_sum nil max_subarray_sum_num nil max_subarray_sum_temp nil] (try (do (when (= (count max_subarray_sum_nums) 0) (throw (ex-info "return" {:v 0.0}))) (set! max_subarray_sum_max_sum 0.0) (set! max_subarray_sum_curr_sum 0.0) (if max_subarray_sum_allow_empty (do (set! max_subarray_sum_max_sum 0.0) (set! max_subarray_sum_curr_sum 0.0) (set! max_subarray_sum_i 0) (while (< max_subarray_sum_i (count max_subarray_sum_nums)) (do (set! max_subarray_sum_num (nth max_subarray_sum_nums max_subarray_sum_i)) (set! max_subarray_sum_temp (+ max_subarray_sum_curr_sum max_subarray_sum_num)) (set! max_subarray_sum_curr_sum (if (> max_subarray_sum_temp 0.0) max_subarray_sum_temp 0.0)) (when (> max_subarray_sum_curr_sum max_subarray_sum_max_sum) (set! max_subarray_sum_max_sum max_subarray_sum_curr_sum)) (set! max_subarray_sum_i (+ max_subarray_sum_i 1))))) (do (set! max_subarray_sum_max_sum (nth max_subarray_sum_nums 0)) (set! max_subarray_sum_curr_sum (nth max_subarray_sum_nums 0)) (set! max_subarray_sum_i 1) (while (< max_subarray_sum_i (count max_subarray_sum_nums)) (do (set! max_subarray_sum_num (nth max_subarray_sum_nums max_subarray_sum_i)) (set! max_subarray_sum_temp (+ max_subarray_sum_curr_sum max_subarray_sum_num)) (set! max_subarray_sum_curr_sum (if (> max_subarray_sum_temp max_subarray_sum_num) max_subarray_sum_temp max_subarray_sum_num)) (when (> max_subarray_sum_curr_sum max_subarray_sum_max_sum) (set! max_subarray_sum_max_sum max_subarray_sum_curr_sum)) (set! max_subarray_sum_i (+ max_subarray_sum_i 1)))))) (throw (ex-info "return" {:v max_subarray_sum_max_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_empty [])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (max_subarray_sum [2.0 8.0 9.0] false)))
      (println (str (max_subarray_sum [0.0 0.0] false)))
      (println (str (max_subarray_sum [(- 1.0) 0.0 1.0] false)))
      (println (str (max_subarray_sum [1.0 2.0 3.0 4.0 (- 2.0)] false)))
      (println (str (max_subarray_sum [(- 2.0) 1.0 (- 3.0) 4.0 (- 1.0) 2.0 1.0 (- 5.0) 4.0] false)))
      (println (str (max_subarray_sum [2.0 3.0 (- 9.0) 8.0 (- 2.0)] false)))
      (println (str (max_subarray_sum [(- 2.0) (- 3.0) (- 1.0) (- 4.0) (- 6.0)] false)))
      (println (str (max_subarray_sum [(- 2.0) (- 3.0) (- 1.0) (- 4.0) (- 6.0)] true)))
      (println (str (max_subarray_sum main_empty false)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
