(ns main (:refer-clojure :exclude [minimum_subarray_sum]))

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

(declare minimum_subarray_sum)

(def ^:dynamic minimum_subarray_sum_curr_sum nil)

(def ^:dynamic minimum_subarray_sum_current_len nil)

(def ^:dynamic minimum_subarray_sum_i nil)

(def ^:dynamic minimum_subarray_sum_left nil)

(def ^:dynamic minimum_subarray_sum_min_len nil)

(def ^:dynamic minimum_subarray_sum_n nil)

(def ^:dynamic minimum_subarray_sum_right nil)

(defn minimum_subarray_sum [minimum_subarray_sum_target minimum_subarray_sum_numbers]
  (binding [minimum_subarray_sum_curr_sum nil minimum_subarray_sum_current_len nil minimum_subarray_sum_i nil minimum_subarray_sum_left nil minimum_subarray_sum_min_len nil minimum_subarray_sum_n nil minimum_subarray_sum_right nil] (try (do (set! minimum_subarray_sum_n (count minimum_subarray_sum_numbers)) (when (= minimum_subarray_sum_n 0) (throw (ex-info "return" {:v 0}))) (when (= minimum_subarray_sum_target 0) (do (set! minimum_subarray_sum_i 0) (while (< minimum_subarray_sum_i minimum_subarray_sum_n) (do (when (= (nth minimum_subarray_sum_numbers minimum_subarray_sum_i) 0) (throw (ex-info "return" {:v 0}))) (set! minimum_subarray_sum_i (+ minimum_subarray_sum_i 1)))))) (set! minimum_subarray_sum_left 0) (set! minimum_subarray_sum_right 0) (set! minimum_subarray_sum_curr_sum 0) (set! minimum_subarray_sum_min_len (+ minimum_subarray_sum_n 1)) (while (< minimum_subarray_sum_right minimum_subarray_sum_n) (do (set! minimum_subarray_sum_curr_sum (+ minimum_subarray_sum_curr_sum (nth minimum_subarray_sum_numbers minimum_subarray_sum_right))) (while (and (>= minimum_subarray_sum_curr_sum minimum_subarray_sum_target) (<= minimum_subarray_sum_left minimum_subarray_sum_right)) (do (set! minimum_subarray_sum_current_len (+ (- minimum_subarray_sum_right minimum_subarray_sum_left) 1)) (when (< minimum_subarray_sum_current_len minimum_subarray_sum_min_len) (set! minimum_subarray_sum_min_len minimum_subarray_sum_current_len)) (set! minimum_subarray_sum_curr_sum (- minimum_subarray_sum_curr_sum (nth minimum_subarray_sum_numbers minimum_subarray_sum_left))) (set! minimum_subarray_sum_left (+ minimum_subarray_sum_left 1)))) (set! minimum_subarray_sum_right (+ minimum_subarray_sum_right 1)))) (if (= minimum_subarray_sum_min_len (+ minimum_subarray_sum_n 1)) 0 minimum_subarray_sum_min_len)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (minimum_subarray_sum 7 [2 3 1 2 4 3])))
      (println (str (minimum_subarray_sum 7 [2 3 (- 1) 2 4 (- 3)])))
      (println (str (minimum_subarray_sum 11 [1 1 1 1 1 1 1 1])))
      (println (str (minimum_subarray_sum 0 [1 2 3])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
